package io.tyoras.cards.shared.endpoint.auth

import cats.syntax.all.*
import dev.profunktor.auth.jwt.JwtToken
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.given
import io.circe.derivation.{Configuration, ConfiguredCodec}
import io.circe.{Decoder, Encoder}
import io.tyoras.cards.domain.auth.{LoginAttempt, Password, UserName}
import io.tyoras.cards.util.validation.StringValidation.*
import io.tyoras.cards.util.validation.syntax.*
import io.tyoras.cards.util.validation.{ParentField, ValidationResult, Validator}

object Payloads:
  given Configuration = Configuration.default.withSnakeCaseMemberNames
  object Request:
    final case class Login(username: Option[String], password: Option[String]) derives ConfiguredCodec
    object Login:
      given Validator[Login, LoginAttempt] = new:
        override def validate(l: Login)(using pf: Option[ParentField]): ValidationResult[LoginAttempt] = (
          l.username.mandatory("username", notBlank, max(100)),
          l.password.mandatory("password", notBlank)
        ).mapN(LoginAttempt.apply)

  object Response:
    given Encoder[JwtToken] = Encoder.encodeString.contramap(_.value)
    given Decoder[JwtToken] = Decoder.decodeString.map(JwtToken(_))
    case class SuccessfulLogin(token: JwtToken, userId: FUUID) derives ConfiguredCodec
