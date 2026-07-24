package io.tyoras.cards.shared.endpoint.auth

import cats.syntax.all.*
import dev.profunktor.auth.jwt.JwtToken
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.given
import io.circe.derivation.ConfiguredCodec
import io.circe.{Decoder, Encoder}
import io.tyoras.cards.domain.auth.model.{LoginAttempt, Password}
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.util.validation.iron.given
import io.tyoras.cards.util.validation.syntax.*
import io.tyoras.cards.util.validation.{ParentField, ValidationResult, Validator}
import io.tyoras.cards.util.codecs.json.given

object Payloads:
  object Request:
    final case class Login(username: Option[String], password: Option[String]) derives ConfiguredCodec
    object Login:
      given Validator[Login, LoginAttempt] = new:
        override def validate(l: Login)(using pf: Option[ParentField]): ValidationResult[LoginAttempt] = (
          l.username.nestedMandatory[User.Name]("username"),
          l.password.nestedMandatory[Password]("password")
        ).mapN(LoginAttempt.apply)

  object Response:
    given Encoder[JwtToken] = Encoder.encodeString.contramap(_.value)
    given Decoder[JwtToken] = Decoder.decodeString.map(JwtToken(_))
    case class SuccessfulLogin(token: JwtToken, userId: FUUID) derives ConfiguredCodec
