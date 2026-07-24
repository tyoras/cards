package io.tyoras.cards.persistence.user

import cats.effect.Sync
import io.chrisdavenport.fuuid.FUUID
import io.github.iltotore.iron.chimney.given
import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.dsl.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.persistence.ParsingError
import io.tyoras.cards.persistence.codecs.skunk.{fuuid, timestampTZ}
import skunk.*
import io.tyoras.cards.persistence.user.UserDAO.Data.given
import skunk.codec.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.scalaland.chimney.partial.syntax.*

import java.time.ZonedDateTime

object UserDAO:

  case class Data(name: String, about: String):
    def toDomain: Either[ParsingError, User.Data] = {
      this.name.transformIntoPartial[User.Name]
      this.transformIntoPartial[User.Data].asEither.left.map(e => ParsingError.InvalidCombination(s"Error while parsing User.Data: ${e.errors}"))
    }

  object Data:
    given PartialTransformer[UserDAO.Data, User.Data] =
      PartialTransformer
        .define[UserDAO.Data, User.Data]
        .withFieldComputedPartial(_.name, n => User.Name.either(n.name).asResult)
        .withFieldComputedPartial(_.about, n => User.About.either(n.about).asResult)
        .buildTransformer
    val codec: Codec[Data]                = (varchar(100) *: varchar).to[Data]
    def fromDomain(user: User.Data): Data = user.transformInto[Data]

  case class Existing(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data):
    def toDomain: Either[ParsingError, User.Existing] =
      this.transformIntoPartial[User.Existing].asEither.left.map(e => ParsingError.InvalidCombination(s"Error while parsing User.Existing: ${e.errors}"))

    def toDomain[F[_] : Sync]: F[User.Existing] = Sync[F].fromEither(this.toDomain)

  object Existing:
    val codec: Codec[Existing]                    = (fuuid *: timestampTZ *: timestampTZ *: Data.codec).to[Existing]
    def fromDomain(user: User.Existing): Existing = user.transformInto[Existing]
