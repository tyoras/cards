package io.tyoras.cards.cli.remote.auth

import cats.effect.{Ref, Sync}
import dev.profunktor.auth.jwt.JwtToken
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.cli.remote.client.AuthClient
import io.tyoras.cards.cli.remote.config.AuthConfig
import cats.syntax.all.*
import io.tyoras.cards.cli.remote.auth.AuthError.UserNotAuthenticated
import org.typelevel.log4cats.LoggerFactory

import scala.util.control.NoStackTrace

final case class AuthCredentials(userId: FUUID, token: JwtToken)
enum AuthError(detail: String) extends Exception(s"Auth error: $detail") with NoStackTrace:
  case LoginFailure(detail: String) extends AuthError("")
  case UserNotAuthenticated         extends AuthError("User is not authenticated")

trait AuthProvider[F[_]]:
  def connectedUserCredentials: F[AuthCredentials]

object AuthProvider:
  def make[F[_] : Sync : LoggerFactory](config: AuthConfig, authClient: AuthClient[F]): F[AuthProvider[F]] =
    val logger                                   = LoggerFactory.getLogger
    val attemptLogin: F[Option[AuthCredentials]] =
      authClient
        .login(config.userName, config.password)
        .flatTap(c => logger.info(s"User ${config.userName}[id = ${c.userId}] authenticated successfully"))
        .map(_.some)
        .handleErrorWith(logger.error(_)(s"Login attempt failed").as(None))

    for
      authRef      <- Ref.of[F, Option[AuthCredentials]](None)
      loginAttempt <- attemptLogin
      _            <- authRef.set(loginAttempt)
    yield new:
      override def connectedUserCredentials: F[AuthCredentials] =
        authRef.get.flatMap(Sync[F].fromOption(_, UserNotAuthenticated))
