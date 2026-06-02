package io.tyoras.cards.server.endpoints.auth

import cats.effect.*
import cats.syntax.all.*
import io.tyoras.cards.domain.auth.AuthService
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.shared.endpoint.auth.Payloads
import io.tyoras.cards.shared.endpoint.auth.Payloads.Request
import io.tyoras.cards.shared.endpoint.auth.Payloads.Request.Login.given
import org.http4s.{HttpRoutes, Response}
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.circe.*
import io.tyoras.cards.util.validation.syntax.*
import org.http4s.circe.CirceEntityCodec.*

object AuthEndpoint:
  def of[F[_] : Async](authService: AuthService[F]): F[Endpoint[F]] = Sync[F].delay {
    new Endpoint[F] with Http4sDsl[F] {

      override val routes: HttpRoutes[F] = Router {
        "login" -> HttpRoutes.of { case r @ POST -> Root =>
          r.as[Request.Login].flatMap(login)
        }
      }

      private def login(loginRequest: Request.Login): F[Response[F]] =
        for
          attempt  <- loginRequest.validateF
          response <- authService
            .login(attempt)
            .flatMap(result => Ok(Payloads.Response.SuccessfulLogin(result.token, result.user.id)))
            .handleErrorWith(_ => Forbidden())
        yield response
    }
  }
