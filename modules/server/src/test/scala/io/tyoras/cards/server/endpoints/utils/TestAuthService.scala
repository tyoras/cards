package io.tyoras.cards.server.endpoints.utils

import io.tyoras.cards.domain.auth.AuthService
import cats.effect.IO
import dev.profunktor.auth.jwt.JwtToken
import io.tyoras.cards.domain.auth.model.{AuthError, LoginAttempt, LoginSuccess}
import io.tyoras.cards.domain.user.model.User
import cats.syntax.all.*
import io.tyoras.cards.domain.auth.model.AuthError.InvalidToken
import pdi.jwt.JwtClaim

class TestAuthService extends AuthService[IO]:
  override def login(attempt: LoginAttempt): IO[LoginSuccess] =
    attempt match
      case la: LoginAttempt if la.userName == TestData.existingUser.name && la.password == TestData.existingUserPassword =>
        LoginSuccess(TestData.existingUserAuthToken, TestData.existingUser).pure
      case la: LoginAttempt if la.userName == TestData.existingUser.name => AuthError.LoginFailure(TestData.existingUser.id, "wrong password").raiseError
      case LoginAttempt(name, _)                                         => AuthError.UnknownUser(name).raiseError

  override def authenticate(jwt: JwtToken): IO[User.Existing] =
    IO.raiseUnless(jwt == TestData.existingUserAuthToken)(InvalidToken("unknown token")).as(TestData.existingUser)

  override def authenticator(jwt: JwtToken): JwtClaim => IO[Option[User.Existing]] = ???
