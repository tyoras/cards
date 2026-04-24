package io.tyoras.cards.server.endpoints.games

import cats.effect.{Async, Sync}
import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.http4s.FUUIDVar
import io.chrisdavenport.fuuid.http4s.implicits.*
import io.circe.Json
import io.tyoras.cards.domain.game.GameService
import io.tyoras.cards.server.endpoints.Endpoint
import org.http4s.circe.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.Http4sDsl
import org.http4s.{AuthedRoutes, Response}
import io.tyoras.cards.domain.user.User
import io.tyoras.cards.shared.endpoint.ErrorPayloads.Response.ApiMessage
import io.tyoras.cards.shared.endpoint.games.Payloads
import io.tyoras.cards.shared.endpoint.games.Payloads.Response.Game.given

import scala.util.chaining.scalaUtilChainingOps

object GameEndpoint:
  def of[F[_] : Async](gameService: GameService[F]): F[Endpoint[F]] = Sync[F].delay {
    new Endpoint[F] with Http4sDsl[F] {

      override val authedRoutes: AuthedRoutes[User.Existing, F] =
        AuthedRoutes.of {
          case GET -> Root / "games" :? UserIdParam(userId) +& FinishedParam(finished) as u => listByUser(userId, finished)
          case GET -> Root / "games" :? FinishedParam(finished) as u                        => listAll(finished)
          case GET -> Root / "games" / FUUIDVar(id) as u                                    => searchById(id)
          case DELETE -> Root / "games" / FUUIDVar(id) as u                                 => deleteById(id)
        }

      object UserIdParam   extends QueryParamDecoderMatcher[FUUID]("user_id")
      object FinishedParam extends FlagQueryParamMatcher("finished")

      private def listByUser(userId: FUUID, finished: Boolean): F[Response[F]] =
        gameService.readManyByUser[Json](userId, finished).map(_.map(Payloads.Response.Game.fromExistingGame)).flatMap(Ok(_))

      private def listAll(finished: Boolean): F[Response[F]] =
        gameService.readAll[Json](finished).map(_.map(Payloads.Response.Game.fromExistingGame)).flatMap(Ok(_))

      private def searchById(id: FUUID): F[Response[F]] =
        gameService.readById[Json](id).flatMap(_.fold(notFoundResponse)(Payloads.Response.Game.fromExistingGame(_).pipe(Ok(_))))

      private def deleteById(id: FUUID): F[Response[F]] =
        gameService.readById[Json](id).flatMap(_.fold(notFoundResponse)(gameService.delete(_) >> NoContent()))

      private val notFoundResponse = NotFound(ApiMessage("not_found", "Requested resource does not exist."))
    }
  }
