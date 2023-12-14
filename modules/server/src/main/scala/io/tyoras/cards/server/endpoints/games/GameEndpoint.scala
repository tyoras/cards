package io.tyoras.cards.server.endpoints.games

import cats.data.NonEmptyList
import cats.effect.{Async, Sync}
import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.http4s.FUUIDVar
import io.chrisdavenport.fuuid.http4s.implicits.*
import io.circe.Json
import io.tyoras.cards.domain.game.{Game, GameService, GameType}
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.server.endpoints.ErrorHandling.ApiMessage
import io.tyoras.cards.server.endpoints.games.Payloads.Request.Creation
import org.http4s.circe.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{EntityDecoder, HttpRoutes, Response, Status}
import io.scalaland.chimney.dsl.*
import io.tyoras.cards.server.endpoints.games.Payloads.Response.Game.fromExisting

import scala.util.chaining.scalaUtilChainingOps

object GameEndpoint:
  def of[F[_] : Async](gameService: GameService[F]): F[Endpoint[F]] = Sync[F].delay {
    new Endpoint[F] with Http4sDsl[F] {

      given EntityDecoder[F, Creation] = accumulatingJsonOf[F, Creation]

      override val routes: HttpRoutes[F] = Router {
        "games" -> HttpRoutes.of {
          case GET -> Root :? UserIdParam(userId) => listByUser(userId)
          case GET -> Root => listAll
          case GET -> Root / FUUIDVar(id) => searchById(id)
          case DELETE -> Root / FUUIDVar(id) => deleteById(id)
        }
      }

      object UserIdParam extends QueryParamDecoderMatcher[FUUID]("user_id")

      private def listByUser(userId: FUUID): F[Response[F]] =
        gameService.readManyByUser[Json](userId).map(_.map(Payloads.Response.Game.fromExistingGame)).flatMap(Ok(_))

      private val listAll: F[Response[F]] =
        gameService.readAll[Json].map(_.map(Payloads.Response.Game.fromExistingGame)).flatMap(Ok(_))

      private def searchById(id: FUUID): F[Response[F]] =
        gameService.readById[Json](id).flatMap(_.fold(notFoundResponse)(_.transformInto[Payloads.Response.Game](fromExisting).pipe(Ok(_))))

      private def deleteById(id: FUUID): F[Response[F]] =
        gameService.readById[Json](id).flatMap(_.fold(notFoundResponse)(gameService.delete(_) >> NoContent()))

      private val notFoundResponse = NotFound(ApiMessage("not_found", "Requested resource does not exist."))
    }
  }
