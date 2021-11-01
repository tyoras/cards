package io.tyoras.cards.server.endpoints.users

import cats.implicits.catsSyntaxTuple2Semigroupal
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe._
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import io.tyoras.cards.domain.user.User
import io.tyoras.cards.domain.user.User.Existing
import io.tyoras.cards.util.validation.StringValidation._
import io.tyoras.cards.util.validation._

import java.time.ZonedDateTime

object Payloads {
  object Request {
    final case class Creation(name: Option[String], about: Option[String])
    object Creation {
      implicit val decoder: Decoder[Creation] = deriveDecoder

      implicit val validator: Validator[Creation, User.Data] = new Validator[Creation, User.Data] {
        override def validate(c: Creation)(implicit pf: Option[ParentField]): ValidationResult[User.Data] = (
          c.name.mandatory("name", notBlank, max(100)),
          c.about.mandatory("about", notBlank)
        ).mapN(User.Data.apply)
      }
    }
  }

  object Response {
    final case class User(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, name: String, about: String)
    object User {
      implicit val encoder: Encoder[User] = deriveEncoder

      def fromExistingUser(user: Existing): Response.User =
        Response.User(user.id, user.createdAt, user.updatedAt, user.name, user.about)
    }
  }

}
