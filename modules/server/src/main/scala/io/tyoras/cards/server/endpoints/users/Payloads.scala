package io.tyoras.cards.server.endpoints.users

import io.chrisdavenport.fuuid.FUUID
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import io.chrisdavenport.fuuid.circe._
import io.scalaland.chimney.Transformer
import io.tyoras.cards.domain.user.User.Existing

import java.time.ZonedDateTime

object Payloads {
  object Request {
    final case class Creation(name: String, about: String)
    object Creation {
      implicit val decoder: Decoder[Creation] = deriveDecoder
    }
  }

  object Response {
    final case class User(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, name: String, about: String)
    object User {
      implicit val encoder: Encoder[User] = deriveEncoder

      implicit val existingToResponse: Transformer[Existing, Response.User] =
        Transformer.define[Existing, Response.User].enableMethodAccessors.buildTransformer
    }
  }

}
