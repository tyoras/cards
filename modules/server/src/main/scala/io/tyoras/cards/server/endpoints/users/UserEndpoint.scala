package io.tyoras.cards.server.endpoints.users

import cats.effect.Async
import cats.syntax.all._
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.http4s.FUUIDVar
import io.tyoras.cards.domain.user.{User, UserService}
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.server.endpoints.users.Payloads.Request.Creation
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{EntityDecoder, HttpRoutes, Response}

object UserEndpoint {
  def of[F[_] : Async](userService: UserService[F]): F[Endpoint[F]] = F.delay {
    new Endpoint[F] with Http4sDsl[F] {

      implicit val inputDecoder: EntityDecoder[F, Creation] = accumulatingJsonOf[F, Creation]

      override val routes: HttpRoutes[F] = Router {
        "users" -> HttpRoutes.of {
          case r @ POST -> Root => r.as[Creation].flatMap(create)
          case r @ PUT -> Root / FUUIDVar(id) => r.as[Creation].flatMap(update(id))
        }
      }

      private def create(payload: Creation): F[Response[F]] =
        userService
          .create(User.Data(payload.name, payload.about))
          .map(created => Payloads.Response.User(created.id, created.createdAt, created.updatedAt, created.name, created.about))
          .flatMap(Created(_))

      private def update(id: FUUID)(payload: Creation): F[Response[F]] =
        userService
          .createOrUpdate(id, User.Data(payload.name, payload.about))
          .map(updated => Payloads.Response.User(updated.id, updated.createdAt, updated.updatedAt, updated.name, updated.about))
          .flatMap(Ok(_))
    }
  }

}
