package io.tyoras.cards.server.endpoints.auth

import org.http4s.Method.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.io.*
import io.tyoras.cards.server.endpoints.EndpointSpec
import io.tyoras.cards.shared.endpoint.auth.Payloads
import io.tyoras.cards.shared.endpoint.auth.Payloads.Request.Login.given
import org.http4s.HttpRoutes
import org.http4s.server.Router
import cats.effect.IO
import io.tyoras.cards.domain.user.model.User
import pl.iterators.baklava.EmptyBody
import cats.syntax.all.*
import dev.profunktor.auth.jwt.JwtToken
import io.tyoras.cards.server.endpoints.utils.{TestAuthService, TestData}
import org.http4s.circe.*

class AuthEndpointSpec extends EndpointSpec:

  override def testedRoutes: HttpRoutes[IO] = Router("auth" -> AuthEndpoint.of[IO](new TestAuthService).unsafeRunSync().routes)

  path(path = "/auth/login")(
    supports(
      POST,
      pathParameters = (),
      description = "Login a user",
      summary = "Authenticate a user"
    )(
      onRequest(body = Payloads.Request.Login(username = "test".some, password = "password".some))
        .respondsWith[Payloads.Response.SuccessfulLogin](Ok, description = "Return user ID and auth token")
        .assert { ctx =>
          val response = ctx.performRequest(testedRoutes)

          response.body shouldBe Payloads.Response.SuccessfulLogin(TestData.existingUserAuthToken, TestData.existingUser.id)
        },
      onRequest(body = Payloads.Request.Login(username = "nonexistent".some, password = "wrongpassword".some))
        .respondsWith[EmptyBody](Forbidden, description = "Return 403 for invalid credentials")
        .assert { ctx =>
          val response = ctx.performRequest(testedRoutes)

          response.status.code shouldBe 403
        }
    )
  )
