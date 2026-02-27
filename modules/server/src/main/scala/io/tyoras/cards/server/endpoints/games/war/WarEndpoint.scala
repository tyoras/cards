package io.tyoras.cards.server.endpoints.games.war

import cats.effect.*
import cats.syntax.all.*
import fs2.{Pipe, Stream}
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.http4s.FUUIDVar
import io.tyoras.cards.domain.game.{Game, GameService, GameType}
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.server.endpoints.games.Payloads
import io.tyoras.cards.server.endpoints.games.Payloads.Response.Game.given
import io.tyoras.cards.server.endpoints.games.war.Payloads.Request.Creation
import org.http4s.circe.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{AuthedRoutes, EntityDecoder, HttpRoutes, Response}
import io.scalaland.chimney.dsl.*
import io.tyoras.cards.domain.game.war.War
import io.tyoras.cards.domain.game.war.given
import io.tyoras.cards.domain.game.war.model.GameState
import io.tyoras.cards.domain.user.User
import io.tyoras.cards.server.endpoints.ErrorHandling.ApiError.ResourceNotFound
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.typelevel.log4cats.LoggerFactory

object WarEndpoint:
  def of[F[_] : Async : LoggerFactory](gameService: GameService[F]): F[Endpoint[F]] = Sync[F].delay {
    new Endpoint[F] with Http4sDsl[F]:

      given EntityDecoder[F, Creation] = accumulatingJsonOf[F, Creation]

      override val authedRoutes: AuthedRoutes[User.Existing, F] = AuthedRoutes.of { case r @ POST -> Root / "games" / "war" as user =>
        r.req.as[Creation].flatMap(create)
      }

      override val wsRoutes: WebSocketBuilder2[F] => HttpRoutes[F] = wsBuilder =>
        Router {
          "games/war" -> HttpRoutes.of { case GET -> Root / FUUIDVar(gameId) =>
            play(gameId, wsBuilder)
          }
        }

      private def create(payload: Creation): F[Response[F]] = for
        war       <- War(payload.players)
        initState <- war.currentState
        created   <- gameService.create(Game.Data[GameState](GameType.War, payload.players, initState))
        response  <- Created(created.transformInto[Payloads.Response.Game](using fromExisting))
      yield response

      private def play(gameId: FUUID, wsBuilder: WebSocketBuilder2[F]): F[Response[F]] =
        for
          readGameData <- gameService.readById[GameState](gameId)
          gameData     <- Sync[F].fromOption(readGameData, ResourceNotFound(s"war game with id $gameId"))
          game         <- War.fromState(gameData.state)
          response     <- wsBuilder.build(wsSend(game), wsReceive(game))
        yield response

      private def wsSend(game: War[F]): Stream[F, WebSocketFrame] = ???

      private def wsReceive(game: War[F]): Pipe[F, WebSocketFrame, Unit] = ???
  }
