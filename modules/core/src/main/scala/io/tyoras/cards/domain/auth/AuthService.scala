package io.tyoras.cards.domain.auth
import cats.effect.Sync
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.domain.user.model.User
import cats.syntax.all.*
import dev.profunktor.auth.jwt.*
import io.circe.parser.decode
import io.tyoras.cards.domain.auth.config.AuthConfig
import io.tyoras.cards.domain.auth.model.{AuthError, LoginAttempt, LoginSuccess, UserClaim}
import io.tyoras.cards.util.crypto.Argon2Hasher
import pdi.jwt.JwtClaim
import pdi.jwt.exceptions.JwtException

// TODO implement logout
trait AuthService[F[_]]:
  def login(attempt: LoginAttempt): F[LoginSuccess]
  def authenticator(jwt: JwtToken): JwtClaim => F[Option[User.Existing]]
  def authenticate(jwt: JwtToken): F[User.Existing]

object AuthService:
  def make[F[_] : Sync](
      userService: UserService[F],
      jwtGenerator: JWTGenerator[F],
      hasher: Argon2Hasher[F],
      userAuthRepository: UserAuthRepository[F],
      authConfig: AuthConfig
  ): F[AuthService[F]] = Sync[F].delay {
    val jwtAuth = JwtAuth.hmac(authConfig.secretKey.toCharArray, authConfig.hmacAlgo)
    new AuthService[F]:
      override def login(attempt: LoginAttempt): F[LoginSuccess] =
        for
          read           <- userService.readByName(attempt.userName)
          user           <- Sync[F].fromOption(read, AuthError.UnknownUser(attempt.userName))
          foundUserAuth  <- userAuthRepository.readByUserId(user.id)
          userAuth       <- Sync[F].fromOption(foundUserAuth, AuthError.LoginFailure(user.id, "missing credentials"))
          hashedPassword <- hasher.hash(attempt.password, userAuth.passwordSalt.some)
          _              <- AuthError.LoginFailure(user.id, "wrong password").raiseError.unlessA(hashedPassword.hash.sameElements(userAuth.passwordHash))
          token          <- jwtGenerator.create(user)
        // TODO store valid token
        yield LoginSuccess(token, user)

      override def authenticator(jwt: JwtToken): JwtClaim => F[Option[User.Existing]] =
        // TODO check if token is stored and known as valid
        (claim: JwtClaim) => decode[UserClaim](claim.content).fold(_ => none[User.Existing].pure, c => userService.readById(c.userId))

      override def authenticate(jwt: JwtToken): F[User.Existing] =
        jwtDecode(jwt, jwtAuth)
          .flatMap(authenticator(jwt))
          .flatMap(Sync[F].fromOption(_, AuthError.UnknownUser(User.Name.applyUnsafe("user from jwt"))))
          .adaptError { case e: JwtException =>
            AuthError.InvalidToken(e.getMessage)
          }
  }
