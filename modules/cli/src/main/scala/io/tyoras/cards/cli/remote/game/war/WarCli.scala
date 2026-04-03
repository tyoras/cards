package io.tyoras.cards.cli.remote.game.war

import cats.effect.implicits.*
import cats.effect.kernel.{Deferred, Ref, Sync}
import cats.effect.std.Console
import cats.effect.{Async, Concurrent, ExitCode}
import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.cli.remote
import io.tyoras.cards.cli.remote.config.parseConfig
import io.tyoras.cards.cli.remote.auth.AuthProvider
import io.tyoras.cards.cli.remote.client.{AuthClient, ChatClient, GamesClient, UsersClient, WarClient}
import io.tyoras.cards.cli.remote.tui.GameStartMenuTUI
import io.tyoras.cards.cli.tui.TUI
import io.tyoras.cards.cli.tui.TUI.Message
import io.tyoras.cards.cli.tui.TUI.Message.Notification
import io.tyoras.cards.domain.game.GameTyp.War
import io.tyoras.cards.domain.game.war.model.GameState
import io.tyoras.cards.shared.protocol.game.OutputMessage as GameOutputMessage
import io.tyoras.cards.shared.protocol.chat.OutputMessage as ChatOutputMessage
import org.http4s.jdkhttpclient.{JdkHttpClient, JdkWSClient}
import org.typelevel.log4cats.*
import io.tyoras.cards.domain.game.war.codecs.given

import java.net.http.HttpClient
import scala.concurrent.duration.DurationInt

trait WarCli[F[_]]:
  def run: F[ExitCode]

object WarCli:
  final case class Config(autoPlay: Boolean)

  val banner: String =
    """ __          __
      | \ \        / /
      |  \ \  /\  / /_ _ _ __
      |   \ \/  \/ / _` | '__|
      |    \  /\  / (_| | |
      |     \/  \/ \__,_|_|   """.stripMargin

  def make[F[_] : Async : Concurrent : LoggerFactory](config: Config)(using console: Console[F]): F[WarCli[F]] =
    for
      config <- parseConfig.onError(e => console.errorln(s"Failed to parse config: $e"))
      (http, webSocket) <- Sync[F].delay(HttpClient.newHttpClient()).map { httpClient =>
        (JdkHttpClient(httpClient), JdkWSClient(httpClient))
      }
      authClient = AuthClient.make(config.client, http)
      authProvider <- AuthProvider.make(config.auth, authClient)
      authedHttpClient = remote.client.authedClient(http, authProvider)
      usersClient      = UsersClient.make(config.client, authedHttpClient)
      chatClient       = ChatClient.make(config.client, webSocket, authProvider)
      gamesClient      = GamesClient.make(config.client, authedHttpClient, webSocket, authProvider)
    yield new WarCliImpl[F](authProvider, usersClient, gamesClient, chatClient)

  private class WarCliImpl[F[_] : Async : Concurrent : LoggerFactory](
      authProvider: AuthProvider[F],
      usersClient: UsersClient[F],
      gamesClient: GamesClient[F],
      chatClient: ChatClient[F]
  ) extends WarCli[F]:
    override val run: F[ExitCode] =
      for
        gameIdRef   <- Deferred[F, FUUID]
        _           <- TUI.runTUI(GameStartMenuTUI.make(War, banner, authProvider, usersClient, gamesClient, gameIdRef))
        gameId      <- gameIdRef.get
        foundById   <- gamesClient.findById(gameId)
        gameInfo    <- Async[F].fromOption(foundById, new IllegalStateException(s"Game $gameId not found"))
        playerNames <- gameInfo.players.toList.parTraverse(usersClient.findById).map(_.flatten.map(u => u.id -> u.name).toMap)
        _ <- gamesClient.connectWarGame(gameId).both(chatClient.connect).use { (warClient, chatClient) =>
          for
            logger <- LoggerFactory[F].create.map(_.addContext(Map("game" -> War.label, "game_id" -> gameId.show)))
            chatRoom = s"War game ${warClient.gameId}"
            _            <- logger.info(s"connected to game ${warClient.gameId}") *> logger.info(s"connected to game chat room : $chatRoom")
            gameStateRef <- Ref.of(None)
            messagesRef  <- Ref.of(List.empty[Message])
            _            <- chatClient.changeRoom(chatRoom)
            gameFiber    <- handleGameOutput(warClient, gameStateRef, messagesRef, logger)
            chatFiber    <- handleChatOutput(chatClient, messagesRef, logger)
            creds        <- authProvider.connectedUserCredentials
            _            <- warClient.getState // load the current state once
            _            <- TUI.runTUI(WarTUI.make(banner, creds.userId, playerNames, gameStateRef, messagesRef, warClient, chatClient))
            _            <- (warClient.quit >> logger.info("Disconnected gracefully")).race(Async[F].sleep(2.second) >> logger.warn("disconnection timeout"))
            _ <- (chatClient.disconnect >> logger.info("Disconnected gracefully from chat"))
              .race(Async[F].sleep(2.second) >> logger.warn("Chat disconnection timeout"))
            _ <- logger.debug("Stop consuming server message stream") >> gameFiber.join >> logger.debug("server message stream stopped")
            _ <- logger.debug("Stop consuming chat message stream") >> chatFiber.join >> logger.debug("chat stream stopped")
          yield ()
        }
      yield ExitCode.Success

    private def handleGameOutput(
        warClient: WarClient[F],
        gameStateRef: Ref[F, Option[GameState]],
        messagesRef: Ref[F, List[Message]],
        logger: SelfAwareStructuredLogger[F]
    ) =
      warClient.streamServerMessages
        .evalTap(m => logger.debug(s"received message:\n$m"))
        .evalTap {
          case GameOutputMessage.GameState(_, _, state) =>
            logger.debug("Received new game state") *>
              Async[F].fromEither(state.as[GameState]).map(_.some).flatMap(gameStateRef.set)
          case GameOutputMessage.GameError(_, _, code, msg) =>
            logger.warn(s"Game error $code : $msg") *>
              messagesRef.update(_ :+ Notification.Error(s"Error $code : $msg"))
          case GameOutputMessage.ProtocolError(_, _, code, msg) =>
            logger.warn(s"Protocol error $code : $msg") *>
              messagesRef.update(_ :+ Notification.Error(s"Error $code : $msg"))
          case GameOutputMessage.PlayerDisconnected(_, id, name) =>
            logger.info(s"Player $name[id = $id] disconnected") *>
              messagesRef.update(_ :+ Notification.Info(s"Player $name has disconnected from the game"))
          case GameOutputMessage.PlayerConnectionSuccess(_, id, name) =>
            logger.info(s"Player $name[id = $id] connected") *>
              messagesRef.update(_ :+ Notification.Info(s"Player $name has join the game"))
          case msg => logger.info(s"$msg ignored")
        }
        .compile
        .drain
        .start

    private def handleChatOutput(chatClient: ChatClient.ConnectedClient[F], messagesRef: Ref[F, List[Message]], logger: SelfAwareStructuredLogger[F[_]]) =
      chatClient.streamServerMessages
        .evalTap {
          case ChatOutputMessage.SendToUser(_, msg)             => messagesRef.update(_ :+ Notification.Info(msg))
          case ChatOutputMessage.ChatMsg(from, _, msg)          => messagesRef.update(_ :+ Message.Chat(from.name, msg))
          case ChatOutputMessage.SuccessfulRegistration(player) => messagesRef.update(_ :+ Notification.Info(s"${player.name} has Joined the chat room"))
          case msg                                              => logger.info(s"Ignored message: $msg")
        }
        .compile
        .drain
        .start
