package io.tyoras.cards.domain.auth.model

import dev.profunktor.auth.jwt.JwtToken
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.*
import io.circe.{Codec, Encoder}
import io.github.iltotore.iron.RefinedSubtype
import io.github.iltotore.iron.constraint.all.*
import io.tyoras.cards.domain.user.model.User

import scala.concurrent.duration.FiniteDuration

type Password = Password.T
object Password
    extends RefinedSubtype[String, DescribedAs[Not[
      Blank
    ] & Trimmed & MinLength[8], "Password must be a non-blank string with a minimum length of 8 characters."]]

type TokenExpiration = TokenExpiration.T
object TokenExpiration extends RefinedSubtype[FiniteDuration, Pure]

final case class UserClaim(userId: FUUID) derives Codec
final case class LoginAttempt(userName: User.Name, password: Password)
final case class LoginSuccess(token: JwtToken, user: User.Existing)
