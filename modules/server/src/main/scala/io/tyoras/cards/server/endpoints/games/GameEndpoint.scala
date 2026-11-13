package io.tyoras.cards.server.endpoints.games

import cats.effect.{Async, Sync}
import cats.syntax.all.*
import io.circe.{Decoder, Json}
import io.tyoras.cards.domain.game.{Game, GameService, GameState}
import io.tyoras.cards.server.endpoints.Endpoint
import org.http4s.circe.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.Http4sDsl
import org.http4s.{AuthedRoutes, EntityEncoder, Response}
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.endpoint.ErrorPayloads.Response.ApiMessage
import io.tyoras.cards.shared.endpoint.games.Payloads
import io.tyoras.cards.shared.endpoint.games.Payloads.Response.Game.given
import pl.iterators.kebs.http4s.*
import pl.iterators.kebs.instances.UtilInstances.uuidFormatter

import scala.util.chaining.scalaUtilChainingOps

object GameEndpoint:
  def of[F[_] : Async](gameService: GameService[F]): F[Endpoint[F]] = Sync[F].delay {
    new Endpoint[F] with Http4sDsl[F] {

      given EntityEncoder[F, fs2.Stream[F, Payloads.Response.Game]] = streamJsonArrayEncoderOf[F, Payloads.Response.Game]

      override val authedRoutes: AuthedRoutes[User.Existing, F] =
        AuthedRoutes.of {
          case GET -> Root / "games" :? UserIdParam(userId) +& FinishedParam(finished) as u => listByUser(userId, finished)
          case GET -> Root / "games" :? FinishedParam(finished) as u                        => listAll(finished)
          case GET -> Root / "games" / WrappedUUID[Game.ID](id) as u                        => searchById(id)
          case DELETE -> Root / "games" / WrappedUUID[Game.ID](id) as u                     => deleteById(id, u.id)
        }

      object UserIdParam   extends QueryParamDecoderMatcher[User.ID]("user_id")
      object FinishedParam extends FlagQueryParamMatcher("finished")

      // hack: avoid to decode to the proper game state when all we need is its json representation
      final private case class RawGameState(json: Json) extends GameState:
        override val code  = "raw"
        override val label = "raw"
      private object RawGameState:
        given Decoder[RawGameState] = Decoder.decodeJson.map(RawGameState(_))

      private def listByUser(userId: User.ID, finished: Boolean): F[Response[F]] =
        gameService.readManyByUser[RawGameState](userId, finished).map(_.map(Payloads.Response.Game.fromExistingGame)).flatMap(Ok(_))

      private def listAll(finished: Boolean): F[Response[F]] =
        Ok(gameService.readAll[RawGameState](finished).map(Payloads.Response.Game.fromExistingGame))

      private def searchById(id: Game.ID): F[Response[F]] =
        gameService.readById[RawGameState](id).flatMap(_.fold(notFoundResponse)(Payloads.Response.Game.fromExistingGame(_).pipe(Ok(_))))

      private def deleteById(gameId: Game.ID, userId: User.ID): F[Response[F]] =
        gameService
          .readById[RawGameState](gameId)
          .flatMap(_.fold(notFoundResponse) { game =>
            if game.data.createdBy == userId then gameService.delete(game) >> NoContent()
            else Forbidden()
          })

      private val notFoundResponse = NotFound(ApiMessage("not_found", "Requested resource does not exist."))
    }
  }
