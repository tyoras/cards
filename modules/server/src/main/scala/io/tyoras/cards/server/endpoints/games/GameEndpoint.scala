package io.tyoras.cards.server.endpoints.games

import cats.effect.{Async, Sync}
import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.http4s.FUUIDVar
import io.chrisdavenport.fuuid.http4s.implicits.*
import io.circe.Json
import io.tyoras.cards.domain.game.GameService
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.server.endpoints.ErrorHandling.ApiMessage
import org.http4s.circe.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.Http4sDsl
import org.http4s.{AuthedRoutes, Response}
import io.scalaland.chimney.dsl.*
import io.tyoras.cards.domain.user.User
import io.tyoras.cards.server.endpoints.games.Payloads.Response.Game.given

import scala.util.chaining.scalaUtilChainingOps

object GameEndpoint:
  def of[F[_] : Async](gameService: GameService[F]): F[Endpoint[F]] = Sync[F].delay {
    new Endpoint[F] with Http4sDsl[F] {

      override val authedRoutes: AuthedRoutes[User.Existing, F] =
        AuthedRoutes.of {
          case GET -> Root / "games" :? UserIdParam(userId) as u => listByUser(userId)
          case GET -> Root / "games" as u                        => listAll
          case GET -> Root / "games" / FUUIDVar(id) as u         => searchById(id)
          case DELETE -> Root / "games" / FUUIDVar(id) as u      => deleteById(id)
        }

      object UserIdParam extends QueryParamDecoderMatcher[FUUID]("user_id")

      private def listByUser(userId: FUUID): F[Response[F]] =
        gameService.readManyByUser[Json](userId).map(_.map(Payloads.Response.Game.fromExistingGame)).flatMap(Ok(_))

      private val listAll: F[Response[F]] =
        gameService.readAll[Json].map(_.map(Payloads.Response.Game.fromExistingGame)).flatMap(Ok(_))

      private def searchById(id: FUUID): F[Response[F]] =
        gameService.readById[Json](id).flatMap(_.fold(notFoundResponse)(Payloads.Response.Game.fromExistingGame(_).pipe(Ok(_))))

      private def deleteById(id: FUUID): F[Response[F]] =
        gameService.readById[Json](id).flatMap(_.fold(notFoundResponse)(gameService.delete(_) >> NoContent()))

      private val notFoundResponse = NotFound(ApiMessage("not_found", "Requested resource does not exist."))
    }
  }
