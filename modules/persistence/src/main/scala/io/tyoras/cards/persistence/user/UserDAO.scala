package io.tyoras.cards.persistence.user

import cats.effect.Sync
import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.dsl.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.persistence.ParsingError
import io.tyoras.cards.persistence.codecs.skunk.timestampTZ
import skunk.*
import io.tyoras.cards.persistence.user.UserDAO.Data.given
import skunk.codec.all.*
import io.scalaland.chimney.partial.syntax.*

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.chaining.scalaUtilChainingOps

object UserDAO:

  case class Data(name: String, about: String):
    lazy val toDomain: Either[ParsingError, User.Data] =
      this.transformIntoPartial[User.Data].asEither.left.map(e => ParsingError.InvalidCombination(s"Error while parsing User.Data: ${e.errors}"))

  object Data:
    given PartialTransformer[UserDAO.Data, User.Data] =
      PartialTransformer
        .define[UserDAO.Data, User.Data]
        .withFieldComputedPartial(_.name, n => User.Name.either(n.name).asResult)
        .withFieldComputedPartial(_.about, n => User.About.either(n.about).asResult)
        .buildTransformer
    val codec: Codec[Data]                = (varchar(100) *: varchar).to[Data]
    def fromDomain(user: User.Data): Data = user.transformInto[Data]

  case class Existing(id: UUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data):
    lazy val toDomain: Either[ParsingError, User.Existing] =
      this
        .intoPartial[User.Existing]
        .withFieldComputed(_.id, _.id.pipe(User.ID(_)))
        .transform
        .asEither
        .left
        .map(e => ParsingError.InvalidCombination(s"Error while parsing User.Existing: ${e.errors}"))

    def toDomainF[F[_] : Sync]: F[User.Existing] = Sync[F].fromEither(this.toDomain)

  object Existing:
    val codec: Codec[Existing]                    = (uuid *: timestampTZ *: timestampTZ *: Data.codec).to[Existing]
    def fromDomain(user: User.Existing): Existing = user.into[Existing].withFieldComputed(_.id, _.id.unwrap).transform
