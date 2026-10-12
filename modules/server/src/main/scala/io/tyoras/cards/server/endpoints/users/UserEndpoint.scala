package io.tyoras.cards.server.endpoints.users

import cats.data.NonEmptyList
import cats.effect.{Async, Sync}
import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.http4s.FUUIDVar
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.server.endpoints.params.given
import io.tyoras.cards.shared.endpoint.users.Payloads.Request.Creation.given
import io.tyoras.cards.shared.endpoint.users.Payloads.Request.PasswordChange.given
import io.tyoras.cards.shared.endpoint.users.Payloads.Request.{Creation, PasswordChange}
import io.tyoras.cards.shared.endpoint.users.Payloads.Response.User.given
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{AuthedRoutes, EntityDecoder, EntityEncoder, HttpRoutes, Response, Status}
import io.tyoras.cards.util.validation.syntax.*
import io.scalaland.chimney.dsl.transformInto
import io.tyoras.cards.domain.auth.UserAuthService
import io.tyoras.cards.domain.auth.model.UserAuthError
import io.tyoras.cards.domain.game.stats.GameStatService
import io.tyoras.cards.shared.endpoint.ErrorPayloads.Response.ApiMessage
import io.tyoras.cards.shared.endpoint.users.Payloads

import scala.util.chaining.scalaUtilChainingOps

object UserEndpoint:
  def of[F[_] : Async](userService: UserService[F], userAuthService: UserAuthService[F], gameStatService: GameStatService[F]): F[Endpoint[F]] = Sync[F].delay {
    new Endpoint[F] with Http4sDsl[F] {

      given EntityDecoder[F, Creation]                              = accumulatingJsonOf[F, Creation]
      given EntityDecoder[F, PasswordChange]                        = accumulatingJsonOf[F, PasswordChange]
      given EntityEncoder[F, fs2.Stream[F, Payloads.Response.User]] = streamJsonArrayEncoderOf[F, Payloads.Response.User]

      override val routes: HttpRoutes[F] = Router {
        "users" -> HttpRoutes.of {
          case r @ POST -> Root                     => r.as[Creation].flatMap(create)
          case GET -> Root :? PartialName(name)     => searchByName(name)
          case GET -> Root                          => listAll
          case GET -> Root / FUUIDVar(id) / "stats" => stats(id)
          case GET -> Root / FUUIDVar(id)           => searchById(id)
        }
      }

      override def authedRoutes: AuthedRoutes[User.Existing, F] = AuthedRoutes.of {
        case r @ PUT -> Root / "users" / FUUIDVar(id) as u                => r.req.as[Creation].flatMap(createOrUpdate(id, u.id))
        case r @ PATCH -> Root / "users" / FUUIDVar(id) / "password" as u => r.req.as[PasswordChange].flatMap(changePassword(id, u.id))
        case DELETE -> Root / "users" / FUUIDVar(id) as u                 => deleteById(id, u.id)
      }

      object PartialName extends QueryParamDecoderMatcher[User.Name]("name")

      private def create(payload: Creation): F[Response[F]] = for
        (validated, password) <- payload.validateF(using v = newUserValidator)
        created               <- userAuthService.createNewUser(validated, password)
        response              <- Created(created.transformInto[Payloads.Response.User])
      yield response

      private def createOrUpdate(userId: FUUID, authenticatedUserId: User.ID)(payload: Creation): F[Response[F]] =
        if userId == authenticatedUserId then
          for
            validated <- payload.validateF(using v = userDataValidator)
            search    <- userService.readById(userId)
            result    <- search.fold(userService.create(validated, withId = userId.some)) { existing =>
              userService.update(existing.copy(data = validated))
            }
            transformed = result.transformInto[Payloads.Response.User]
            status      = search.fold(Status.Created)(_ => Status.Ok)
            response    = Response[F](status).withEntity(transformed)
          yield response
        else Forbidden()

      private def searchByName(name: User.Name): F[Response[F]] =
        userService.readManyByPartialName(name).map(_.transformInto[List[Payloads.Response.User]]).flatMap(Ok(_))

      private val listAll: F[Response[F]] =
        Ok(userService.readAll.map(_.transformInto[Payloads.Response.User]))

      private def searchById(id: FUUID): F[Response[F]] =
        userService.readById(id).flatMap(_.fold(notFoundResponse)(_.transformInto[Payloads.Response.User].pipe(Ok(_))))

      private def deleteById(deletedUserId: FUUID, authenticatedUserId: User.ID): F[Response[F]] =
        if deletedUserId == authenticatedUserId then userService.readById(deletedUserId).flatMap(_.fold(notFoundResponse)(userService.delete(_) *> NoContent()))
        else Forbidden()

      private def stats(id: FUUID): F[Response[F]] =
        userService
          .readById(id)
          .flatMap(
            _.fold(notFoundResponse)(_ =>
              for
                userStats <- gameStatService.getPlayerStats(id)
                response  <- userStats.traverse(_.stats.transformInto[NonEmptyList[Payloads.Response.UserGameStats]]).pipe(Ok(_))
              yield response
            )
          )

      private def changePassword(userId: User.ID, authenticatedUserId: User.ID)(req: PasswordChange): F[Response[F]] =
        if userId == authenticatedUserId then
          given User.ID = userId
          for
            request  <- req.validateF
            response <- (userAuthService.changeUserPassword(userId, request.oldPassword, request.newPassword) *> NoContent()).handleErrorWith {
              case UserAuthError.UserNotFound(_)    => notFoundResponse
              case UserAuthError.InvalidOldPassword =>
                BadRequest(ApiMessage("invalid_old_password", "The provided old password is not the user's current password"))
            }
          yield response
        else Forbidden()

      private val notFoundResponse = NotFound(ApiMessage("not_found", "Requested resource does not exist."))
    }
  }
