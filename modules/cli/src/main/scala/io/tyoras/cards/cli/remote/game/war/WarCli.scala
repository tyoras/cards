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
import io.tyoras.cards.cli.remote.client.{AuthClient, GamesClient, UsersClient}
import io.tyoras.cards.cli.remote.tui.GameStartMenuTUI
import io.tyoras.cards.cli.tui.TUI
import io.tyoras.cards.cli.tui.TUI.Notification
import io.tyoras.cards.domain.game.GameTyp.War
import io.tyoras.cards.domain.game.war.model.GameState
import io.tyoras.cards.shared.protocol.game.OutputMessage
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

  def apply[F[_] : Async : Concurrent : LoggerFactory](config: Config)(using console: Console[F]): WarCli[F] =
    new WarCli[F]:
      override val run: F[ExitCode] =
        for
          config <- parseConfig("cli.conf").onError(e => console.errorln(s"Failed to parse config: $e"))
          (http, webSocket) <- Sync[F].delay(HttpClient.newHttpClient()).map { httpClient =>
            (JdkHttpClient(httpClient), JdkWSClient(httpClient))
          }
          authClient = AuthClient.make(config.client, http)
          authProvider <- AuthProvider.make(config.auth, authClient)
          authedHttpClient = remote.client.authedClient(http, authProvider)
          usersClient      = UsersClient.make(config.client, authedHttpClient)
          gamesClient      = GamesClient.make(config.client, authedHttpClient, webSocket, authProvider)
          gameIdRef   <- Deferred[F, FUUID]
          _           <- TUI.runTUI(GameStartMenuTUI.make(War, banner, authProvider, usersClient, gamesClient, gameIdRef))
          gameId      <- gameIdRef.get
          foundById   <- gamesClient.findById(gameId)
          gameInfo    <- Async[F].fromOption(foundById, new IllegalStateException(s"Game $gameId not found"))
          playerNames <- gameInfo.players.toList.parTraverse(usersClient.findById).map(_.flatten.map(u => u.id -> u.name).toMap)
          _ <- gamesClient.connectWarGame(gameId).use { warClient =>
            for
              logger       <- LoggerFactory[F].create.map(_.addContext(Map("game" -> War.label, "game_id" -> gameId.show)))
              _            <- logger.info(s"connected to game ${warClient.gameId}")
              gameStateRef <- Ref.of(None)
              notifRef     <- Ref.of(None)
              fiber <- warClient.streamServerMessages
                .evalTap(m => logger.debug(s"received message:\n$m"))
                .evalTap {
                  case OutputMessage.GameState(_, _, state) =>
                    logger.debug("Received new game state") *>
                      Async[F].fromEither(state.as[GameState]).map(_.some).flatMap(gameStateRef.set)
                  case OutputMessage.GameError(_, _, code, msg) =>
                    logger.warn(s"Game error $code : $msg") *>
                      notifRef.set(Notification.Error(s"Error $code : $msg").some)
                  case OutputMessage.ProtocolError(_, _, code, msg) =>
                    logger.warn(s"Protocol error $code : $msg") *>
                      notifRef.set(Notification.Error(s"Error $code : $msg").some)
                  case OutputMessage.PlayerDisconnected(_, id, name) =>
                    logger.info(s"Player $name[id = $id] disconnected") *>
                      notifRef.set(Notification.Info(s"Player $name has disconnected from the game").some)
                  case OutputMessage.PlayerConnectionSuccess(_, id, name) =>
                    logger.info(s"Player $name[id = $id] connected") *>
                      notifRef.set(Notification.Info(s"Player $name has join the game").some)
                  case msg => logger.info(s"$msg ignored")
                }
                .compile
                .drain
                .start
              creds <- authProvider.connectedUserCredentials
              _     <- warClient.getState // load the current state once
              _     <- TUI.runTUI(WarTUI.make(banner, creds.userId, playerNames, gameStateRef, notifRef, warClient))
              _     <- (warClient.quit >> logger.info("Disconnected gracefully")).race(Async[F].sleep(2.second) >> logger.warn("disconnection timeout"))
              _     <- logger.debug("Stop consuming server message stream") >> fiber.join >> logger.debug("stream stopped")
            yield ()
          }
          exitCode <- ExitCode.Success.pure
        yield exitCode
