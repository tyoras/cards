package io.tyoras.cards.server.endpoints.users

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._

import java.time.ZonedDateTime
import java.util.UUID

object Payloads {
  object Request {
    final case class Creation(name: String, about: String)
    object Creation {
      implicit val decoder: Decoder[Creation] = deriveDecoder
    }
  }

  object Response {
    final case class User(id: UUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, name: String, about: String)
    object User {
      implicit val encoder: Encoder[User] = deriveEncoder
    }
  }

}
