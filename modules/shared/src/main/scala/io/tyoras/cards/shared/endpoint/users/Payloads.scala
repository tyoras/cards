package io.tyoras.cards.shared.endpoint.users

import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.*
import io.circe.derivation.ConfiguredCodec
import io.circe.{Codec, Decoder, Encoder}
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.tyoras.cards.domain.user.model.User as DomainUser
import io.tyoras.cards.util.validation.*
import io.tyoras.cards.util.validation.iron.given
import io.tyoras.cards.util.validation.syntax.*
import io.tyoras.cards.util.codecs.json.given
import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.cats.*
import io.scalaland.chimney.partial.syntax.*
import io.github.iltotore.iron.cats.*

import java.time.ZonedDateTime

object Payloads:
  object Request:
    final case class Creation(name: Option[String], about: Option[String]) derives Codec
    object Creation:
      given Validator[Creation, DomainUser.Data] = new:
        override def validate(c: Creation)(using pf: Option[ParentField]): ValidationResult[DomainUser.Data] = (
          c.name.nestedMandatory[DomainUser.Name]("name"),
          c.about.nestedMandatory[DomainUser.About]("about")
        ).mapN(DomainUser.Data.apply)

  object Response:
    final case class User(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, name: String, about: String) derives ConfiguredCodec
    object User:
      given Transformer[DomainUser.Existing, Response.User] =
        Transformer.define[DomainUser.Existing, Response.User].enableMethodAccessors.buildTransformer

      given PartialTransformer[Response.User, DomainUser.Existing] =
        PartialTransformer
          .define[Response.User, DomainUser.Existing]
          .withFieldComputedPartial(
            _.data,
            resp =>
              for
                name  <- DomainUser.Name.validatedNec(resp.name).asResult
                about <- DomainUser.About.validatedNec(resp.about).asResult
              yield DomainUser.Data(name, about)
          )
          .buildTransformer
