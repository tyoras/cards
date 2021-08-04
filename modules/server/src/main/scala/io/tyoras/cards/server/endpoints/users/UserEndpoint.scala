package io.tyoras.cards.server.endpoints.users

import cats.effect.Async
import cats.syntax.all._
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.http4s.FUUIDVar
import io.scalaland.chimney.dsl.TransformerOps
import io.tyoras.cards.domain.user.{User, UserService}
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.server.endpoints.users.Payloads.Request.Creation
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{EntityDecoder, HttpRoutes, Response}

import scala.util.chaining.scalaUtilChainingOps

object UserEndpoint {
  def of[F[_] : Async](userService: UserService[F]): F[Endpoint[F]] = F.delay {
    new Endpoint[F] with Http4sDsl[F] {

      implicit val inputDecoder: EntityDecoder[F, Creation] = accumulatingJsonOf[F, Creation]

      override val routes: HttpRoutes[F] = Router {
        "users" -> HttpRoutes.of {
          case r @ POST -> Root => r.as[Creation].flatMap(create)
          case r @ PUT -> Root / FUUIDVar(id) => r.as[Creation].flatMap(update(id))
          case GET -> Root :? PartialName(name) => searchByName(name)
          case GET -> Root => listAll
          case GET -> Root / FUUIDVar(id) => searchById(id)
          case DELETE -> Root / FUUIDVar(id) => deleteById(id)
        }
      }

      object PartialName extends QueryParamDecoderMatcher[String]("name")

      private def create(payload: Creation): F[Response[F]] =
        userService.create(payload.transformInto[User.Data]).map(_.transformInto[Payloads.Response.User]).flatMap(Created(_))

      private def update(id: FUUID)(payload: Creation): F[Response[F]] =
        userService.createOrUpdate(id, payload.transformInto[User.Data]).map(_.transformInto[Payloads.Response.User]).flatMap(Ok(_))

      private def searchByName(name: String): F[Response[F]] =
        userService.readManyByName(name).map(_.transformInto[List[Payloads.Response.User]]).flatMap(Ok(_))

      private val listAll: F[Response[F]] =
        userService.readAll.map(_.transformInto[List[Payloads.Response.User]]).flatMap(Ok(_))

      private def searchById(id: FUUID): F[Response[F]] =
        userService.readById(id).flatMap(_.fold(NotFound())(_.transformInto[Payloads.Response.User].pipe(Ok(_))))

      private def deleteById(id: FUUID): F[Response[F]] =
        userService.readById(id).flatMap(_.fold(NotFound())(userService.delete(_) >> NoContent()))
    }
  }

}
