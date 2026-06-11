package io.tyoras.cards.domain.auth.model

import dev.profunktor.auth.jwt.JwtToken
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.*
import io.circe.{Codec, Encoder}
import io.tyoras.cards.domain.user.model.User

import scala.concurrent.duration.FiniteDuration

type UserName        = String
type Password        = String
type TokenExpiration = FiniteDuration

final case class UserClaim(userId: FUUID) derives Codec
final case class LoginAttempt(userName: UserName, password: Password)
final case class LoginSuccess(token: JwtToken, user: User.Existing)
