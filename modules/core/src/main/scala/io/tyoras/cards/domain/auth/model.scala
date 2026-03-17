package io.tyoras.cards.domain.auth

import dev.profunktor.auth.jwt.JwtToken
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.*
import io.circe.{Codec, Encoder}
import io.tyoras.cards.domain.user.User
import pdi.jwt.JwtAlgorithm
import pdi.jwt.algorithms.JwtHmacAlgorithm

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NoStackTrace

type UserName        = String
type Password        = String
type TokenExpiration = FiniteDuration

final case class AuthConfig(secretKey: String, exp: TokenExpiration):
  val hmacAlgo: JwtHmacAlgorithm = JwtAlgorithm.HS256

final case class UserClaim(userId: FUUID) derives Codec
final case class LoginAttempt(userName: UserName, password: Password)
final case class LoginSuccess(token: JwtToken, user: User.Existing)

enum AuthError(val message: String) extends Exception(message) with NoStackTrace:
  case InvalidToken(detail: String)    extends AuthError(s"Invalid token: $detail")
  case UnknownUser(userName: UserName) extends AuthError(s"$userName is unknown")
