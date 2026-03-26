package io.tyoras.cards.server.endpoints.games.war

import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.implicits.*
import cats.effect.kernel.Ref
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.concurrent.Topic
import fs2.{Pipe, Stream}
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.given
import io.tyoras.cards.domain.game.{Game, GameService, GameTyp}
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.shared.endpoint.games.Payloads.Response.Game.given
import io.tyoras.cards.shared.endpoint.games.war.Payloads.Request.Creation
import org.http4s.circe.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{AuthedRoutes, EntityDecoder, HttpRoutes, Response}
import io.tyoras.cards.shared.protocol.game.OutputMessage.{DiscardMessage, KeepAlive, PlayerConnectionSuccess, PlayerDisconnected}
import io.tyoras.cards.domain.game.war.War
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.domain.game.war.model.GameState
import io.tyoras.cards.domain.user.{User, UserService}
import io.tyoras.cards.server.endpoints.ErrorHandling.ApiError.InvalidRequest
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.typelevel.log4cats.LoggerFactory

import scala.concurrent.duration.DurationInt
import io.circe.syntax.given
import io.tyoras.cards.server.protocol.chat.{ChatProtocol, ChatUser, Room}
import io.tyoras.cards.server.protocol.game.{ConnectedPlayer, GameProtocol, InputParser}
import io.tyoras.cards.shared.endpoint.games.Payloads
import io.tyoras.cards.shared.protocol.game.OutputMessage

object WarEndpoint:
  def make[F[_] : Async : Concurrent : Temporal : LoggerFactory](
      gameService: GameService[F],
      userService: UserService[F],
      gameProtocol: GameProtocol[F],
      chatProtocol: ChatProtocol[F]
  ): Resource[F, Endpoint[F]] =
    for
      // needed to order the command as FIFO
      outputQueue <- Resource.eval(Queue.unbounded[F, OutputMessage])
      outputTopic <- Resource.eval(Topic[F, OutputMessage])
      _ <- Stream
        .fromQueueUnterminated(outputQueue)
        .through(outputTopic.publish)
        .concurrently(
          // heartbeat to prevent network timeout
          Stream.awakeEvery[F](30.seconds).as(KeepAlive).through(outputTopic.publish)
        )
        .compile
        .drain
        .background
      inputParser = InputParser.make(gameProtocol)
    yield new Endpoint[F] with Http4sDsl[F]:
      given EntityDecoder[F, Creation] = accumulatingJsonOf[F, Creation]

      override val authedRoutes: AuthedRoutes[User.Existing, F] = AuthedRoutes.of {
        case r @ POST -> Root / "games" / "war" as user           => r.req.as[Creation].flatMap(create)
        case r @ GET -> Root / "games" / "war" / "active" as user => listPlayerGames(user)
      }

      override val wsRoutes: WebSocketBuilder2[F] => HttpRoutes[F] = wsBuilder =>
        Router {
          "games/war" -> HttpRoutes.of { case GET -> Root =>
            play(wsBuilder)
          }
        }

      private def create(payload: Creation): F[Response[F]] = for
        players   <- playersValidation(payload.players)
        war       <- War(payload.players)
        initState <- war.currentState
        created   <- gameService.create(Game.Data[GameState](GameTyp.War, payload.players, initState))
        _         <- gameProtocol.registerActiveGame(created.id, war)
        _         <- createGameChat(created.id, players)
        response  <- Created(Payloads.Response.Game.fromExistingGame(created))
      yield response

      private def playersValidation(playerIds: NonEmptyList[FUUID]): F[List[User.Existing]] =
        for
          players <- playerIds.traverse(userService.readById)
          _       <- InvalidRequest("Impossible to create a new War game because some players are unknown").raiseError.whenA(players.exists(_.isEmpty))
        yield players.toList.flatten

      private def createGameChat(gameId: FUUID, players: List[User.Existing]): F[Unit] =
        Sync[F].fromEither(Room(s"War game $gameId").toEither.leftMap(_ => new IllegalStateException("Invalid room id"))).flatMap { chatRoom =>
          players.traverse_(player => chatProtocol.enterRoom(ChatUser(player.name, player.id.some), chatRoom))
        }

      private def listPlayerGames(player: User.Existing): F[Response[F]] =
        for
          games       <- gameProtocol.currentState
          playerGames <- games.warGames.toList.filterA { case (gameId, game) => game.playerIds.map(_.exists(_ == player.id)) }.map(_.map(_._1))
          response    <- Ok(playerGames)
        yield response

      private def play(wsBuilder: WebSocketBuilder2[F]): F[Response[F]] =
        for
          playerRef             <- Ref.of[F, Option[ConnectedPlayer]](None)
          notAuthenticatedQueue <- Queue.unbounded[F, OutputMessage]
          response              <- wsBuilder.build(wsSend(playerRef, notAuthenticatedQueue), wsReceive(playerRef, notAuthenticatedQueue))
        yield response

      private def wsSend(playerRef: Ref[F, Option[ConnectedPlayer]], notAuthenticatedQueue: Queue[F, OutputMessage]): Stream[F, WebSocketFrame] =
        def notAuthenticatedStream = Stream
          .fromQueueUnterminated(notAuthenticatedQueue)
          .filter {
            case DiscardMessage => false
            case _              => true
          }
          .map(processMsg)

        def mainStream = outputTopic.subscribe(maxQueued = 1000).evalFilter(filterMsg(_, playerRef)).map(processMsg)

        Stream(notAuthenticatedStream, mainStream).parJoinUnbounded

      private def filterMsg(msg: OutputMessage, playerRef: Ref[F, Option[ConnectedPlayer]]): F[Boolean] =
        msg match
          case DiscardMessage                                       => false.pure
          case OutputMessage.GameState(gameId, recipient, _)        => playerRef.get.map(_.fold(false)(p => p.gameId == gameId && p.playerId == recipient))
          case OutputMessage.GameError(gameId, recipient, _, _)     => playerRef.get.map(_.fold(false)(p => p.gameId == gameId && p.playerId == recipient))
          case OutputMessage.ProtocolError(gameId, recipient, _, _) => playerRef.get.map(_.fold(false)(p => p.gameId == gameId && p.playerId == recipient))
          case PlayerConnectionSuccess(gameId, _, _)                => playerRef.get.map(_.fold(false)(_.gameId == gameId))
          case PlayerDisconnected(gameId, _, _)                     => playerRef.get.map(_.fold(false)(_.gameId == gameId))
          case _                                                    => true.pure

      private def processMsg(msg: OutputMessage): WebSocketFrame =
        msg match
          case KeepAlive => WebSocketFrame.Ping()
          case _         => WebSocketFrame.Text(msg.asJson.noSpaces)

      private def wsReceive(
          playerRef: Ref[F, Option[ConnectedPlayer]],
          unAuthenticatedQueue: Queue[F, OutputMessage]
      ): Pipe[F, WebSocketFrame, Unit] =
        handleWebSocketStream(_, playerRef)
          .evalMap { m =>
            playerRef.get.flatMap {
              case Some(_) => outputQueue.offer(m)
              case None    => unAuthenticatedQueue.offer(m)
            }
          }
          .concurrently {
            Stream.awakeEvery(30.seconds).as(KeepAlive).foreach(unAuthenticatedQueue.offer)
          }

      private def handleWebSocketStream(frameStream: Stream[F, WebSocketFrame], playerRef: Ref[F, Option[ConnectedPlayer]]): Stream[F, OutputMessage] =
        for
          frame <- frameStream
          outputMessage <- Stream.evalSeq(
            frame match
              case WebSocketFrame.Text(text, _) => inputParser.parse(playerRef, text)
              // at the moment there is a known bug in ember-server that prevent the stream to know about the close frame (see https://github.com/http4s/http4s/issues/6806)
              case WebSocketFrame.Close(_) => gameProtocol.disconnect(playerRef).map(List(_))
          )
        yield outputMessage
