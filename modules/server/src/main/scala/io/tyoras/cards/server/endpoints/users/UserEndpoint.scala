package io.tyoras.cards.server.endpoints.users

import cats.effect.Async
import cats.syntax.all._
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.http4s.FUUIDVar
import io.scalaland.chimney.dsl.TransformerOps
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.server.endpoints.ErrorHandling.ApiMessage
import io.tyoras.cards.server.endpoints.users.Payloads.Request.Creation
import io.tyoras.cards.util.validation.ValidationOps
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{EntityDecoder, HttpRoutes, Response, Status}

import scala.util.chaining.scalaUtilChainingOps

object UserEndpoint {
  def of[F[_] : Async](userService: UserService[F]): F[Endpoint[F]] = F.delay {
    new Endpoint[F] with Http4sDsl[F] {

      implicit val inputDecoder: EntityDecoder[F, Creation] = accumulatingJsonOf[F, Creation]

      override val routes: HttpRoutes[F] = Router {
        "users" -> HttpRoutes.of {
          case r @ POST -> Root => r.as[Creation].flatMap(create)
          case r @ PUT -> Root / FUUIDVar(id) => r.as[Creation].flatMap(createOrUpdate(id))
          case GET -> Root :? PartialName(name) => searchByName(name)
          case GET -> Root => listAll
          case GET -> Root / FUUIDVar(id) => searchById(id)
          case DELETE -> Root / FUUIDVar(id) => deleteById(id)
        }
      }

      object PartialName extends QueryParamDecoderMatcher[String]("name")

      private def create(payload: Creation): F[Response[F]] = for {
        validated <- payload.validateF
        created   <- userService.create(validated)
        response  <- Created(created.transformInto[Payloads.Response.User])
      } yield response

      private def createOrUpdate(id: FUUID)(payload: Creation): F[Response[F]] = for {
        validated <- payload.validateF
        search    <- userService.readById(id)
        result <- search.fold(userService.create(validated, withId = id.some)) { existing =>
          userService.update(existing.copy(data = validated))
        }
        transformed = result.transformInto[Payloads.Response.User]
        status = search.fold(Status.Created)(_ => Status.Ok)
        response = Response[F](status).withEntity(transformed)
      } yield response

      private def searchByName(name: String): F[Response[F]] =
        userService.readManyByName(name).map(_.transformInto[List[Payloads.Response.User]]).flatMap(Ok(_))

      private val listAll: F[Response[F]] =
        userService.readAll.map(_.transformInto[List[Payloads.Response.User]]).flatMap(Ok(_))

      private def searchById(id: FUUID): F[Response[F]] =
        userService.readById(id).flatMap(_.fold(notFoundResponse)(_.transformInto[Payloads.Response.User].pipe(Ok(_))))

      private def deleteById(id: FUUID): F[Response[F]] =
        userService.readById(id).flatMap(_.fold(notFoundResponse)(userService.delete(_) >> NoContent()))

      private val notFoundResponse = NotFound(ApiMessage("not_found", "Requested resource does not exist."))
    }
  }

}
