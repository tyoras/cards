package io.tyoras.cards.domain.auth
import cats.effect.Sync
import io.tyoras.cards.domain.user.{User, UserService}
import cats.syntax.all.*
import dev.profunktor.auth.jwt.*
import io.circe.parser.decode
import pdi.jwt.JwtClaim

// TODO implement logout
trait AuthService[F[_]]:
  // TODO implement proper login
  def login(attempt: LoginAttempt): F[JwtToken]
  def authenticate(jwt: JwtToken): JwtClaim => F[Option[User.Existing]]

object AuthService:
  // simple implementation that always return a valid token when the user is known
  // does not actually check the password
  def naive[F[_] : Sync](userService: UserService[F], jwtGenerator: JWTGenerator[F]): F[AuthService[F]] = Sync[F].delay {
    new AuthService[F]:
      override def login(attempt: LoginAttempt): F[JwtToken] =
        for
          read  <- userService.readByName(attempt.userName.trim)
          user  <- Sync[F].fromOption(read, AuthError.UnknownUser(attempt.userName))
          token <- jwtGenerator.create(user)
        yield token

      override def authenticate(jwt: JwtToken): JwtClaim => F[Option[User.Existing]] =
        (claim: JwtClaim) => decode[UserClaim](claim.content).fold(_ => none[User.Existing].pure, c => userService.readById(c.userId))
  }
