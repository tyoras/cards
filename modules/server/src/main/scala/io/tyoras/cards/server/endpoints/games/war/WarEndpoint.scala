package io.tyoras.cards.server.endpoints.games.war

import cats.effect.{Async, Sync}
import cats.syntax.all.*
import io.tyoras.cards.domain.game.{Game, GameService, GameType}
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.server.endpoints.games.Payloads
import io.tyoras.cards.server.endpoints.games.Payloads.Response.Game.given
import io.tyoras.cards.server.endpoints.games.war.Payloads.Request.Creation
import org.http4s.circe.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{EntityDecoder, HttpRoutes, Response}
import io.scalaland.chimney.dsl.*
import io.tyoras.cards.domain.game.war.War
import io.tyoras.cards.domain.game.war.given
import io.tyoras.cards.domain.game.war.model.GameContext
import org.typelevel.log4cats.LoggerFactory

object WarEndpoint:
  def of[F[_] : Async : LoggerFactory](gameService: GameService[F]): F[Endpoint[F]] = Sync[F].delay {
    new Endpoint[F] with Http4sDsl[F]:

      given EntityDecoder[F, Creation] = accumulatingJsonOf[F, Creation]

      override val routes: HttpRoutes[F] = Router {
        "games/war" -> HttpRoutes.of { case r @ POST -> Root =>
          r.as[Creation].flatMap(create)
        }
      }

      private def create(payload: Creation): F[Response[F]] = for
        war       <- War(payload.players)
        initState <- war.currentState
        // TODO persist the game state instead of the context
        created  <- gameService.create(Game.Data[GameContext](GameType.War, payload.players, initState.context))
        response <- Created(created.transformInto[Payloads.Response.Game](using fromExisting))
      yield response
  }
