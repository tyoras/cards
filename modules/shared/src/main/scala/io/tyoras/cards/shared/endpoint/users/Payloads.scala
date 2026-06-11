package io.tyoras.cards.shared.endpoint.users

import cats.implicits.catsSyntaxTuple2Semigroupal
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.*
import io.circe.derivation.ConfiguredCodec
import io.circe.{Codec, Decoder, Encoder}
import io.scalaland.chimney.Transformer
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.domain.user.model.User.{Data, Existing}
import io.tyoras.cards.util.validation.*
import io.tyoras.cards.util.validation.StringValidation.*
import io.tyoras.cards.util.validation.syntax.*
import io.tyoras.cards.util.codecs.json.given

import java.time.ZonedDateTime

object Payloads:
  object Request:
    final case class Creation(name: Option[String], about: Option[String]) derives Codec
    object Creation:
      given Validator[Creation, User.Data] = new:
        override def validate(c: Creation)(using pf: Option[ParentField]): ValidationResult[User.Data] = (
          c.name.mandatory("name", notBlank, max(100)),
          c.about.mandatory("about", notBlank)
        ).mapN(User.Data.apply)

  object Response:
    final case class User(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, name: String, about: String) derives ConfiguredCodec
    object User:
      given Transformer[Existing, Response.User] =
        Transformer.define[Existing, Response.User].enableMethodAccessors.buildTransformer
      given Transformer[Response.User, Existing] =
        Transformer.define[Response.User, Existing].withFieldComputed(_.data, resp => Data(name = resp.name, about = resp.about)).buildTransformer
