package io.tyoras.cards.server.endpoints.auth

import io.circe.{Decoder, Encoder}
import io.circe.derivation.{Configuration, ConfiguredDecoder}
import io.tyoras.cards.domain.auth.{LoginAttempt, Password, UserName}
import io.tyoras.cards.util.validation.{ParentField, ValidationResult, Validator}
import io.tyoras.cards.util.validation.syntax.*
import io.tyoras.cards.util.validation.StringValidation.*
import cats.syntax.all.*
import dev.profunktor.auth.jwt.JwtToken

object Payloads:
  given Configuration = Configuration.default.withSnakeCaseMemberNames
  object Request:
    final case class Login(username: Option[String], password: Option[String])
    object Login:
      given Decoder[Login] = ConfiguredDecoder.derived

      given Validator[Login, LoginAttempt] = new:
        override def validate(l: Login)(using pf: Option[ParentField]): ValidationResult[LoginAttempt] = (
          l.username.mandatory("username", notBlank, max(100)),
          l.password.mandatory("password", notBlank)
        ).mapN(LoginAttempt.apply)

  object Response:
    given Encoder[JwtToken] = Encoder.forProduct1("access_token")(_.value)
