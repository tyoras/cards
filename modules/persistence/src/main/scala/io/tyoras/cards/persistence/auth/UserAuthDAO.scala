package io.tyoras.cards.persistence.auth

import cats.effect.Sync
import io.tyoras.cards.domain.auth.model.UserAuth
import io.tyoras.cards.persistence.ParsingError
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.partial.syntax.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.persistence.codecs.skunk.timestampTZ
import skunk.*
import skunk.codec.all.*
import io.tyoras.cards.persistence.auth.UserAuthDAO.Data.given

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.chaining.*

object UserAuthDAO:

  case class Data(userId: UUID, passwordHash: Array[Byte], passwordSalt: Array[Byte]):
    lazy val toDomain: Either[ParsingError, UserAuth.Data] =
      this.transformIntoPartial[UserAuth.Data].asEither.left.map(e => ParsingError.InvalidCombination(s"Error while parsing UserAuth.Data: ${e.errors}"))

  object Data:
    given PartialTransformer[Data, UserAuth.Data] =
      PartialTransformer
        .define[Data, UserAuth.Data]
        .withFieldComputedPartial(_.passwordHash, h => UserAuth.PasswordHashBytes.either(h.passwordHash).asResult)
        .withFieldComputedPartial(_.passwordSalt, s => UserAuth.PasswordSaltBytes.either(s.passwordSalt).asResult)
        .withFieldComputed(_.userId, _.userId.pipe(User.ID(_)))
        .buildTransformer
    val codec: Codec[Data]                 = (uuid *: bytea *: bytea).to[Data]
    given Transformer[UserAuth.Data, Data] =
      Transformer.define[UserAuth.Data, Data].withFieldComputed(_.userId, _.userId.unwrap).buildTransformer
    def fromDomain(userAuth: UserAuth.Data): Data = userAuth.transformInto[Data]

  case class Existing(createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data):
    lazy val toDomain: Either[ParsingError, UserAuth.Existing] =
      this
        .transformIntoPartial[UserAuth.Existing]
        .asEither
        .left
        .map(e => ParsingError.InvalidCombination(s"Error while parsing UserAuth.Existing: ${e.errors}"))

    def toDomainF[F[_] : Sync]: F[UserAuth.Existing] = Sync[F].fromEither(this.toDomain)

  object Existing:
    val codec: Codec[Existing]                            = (timestampTZ *: timestampTZ *: Data.codec).to[Existing]
    def fromDomain(userAuth: UserAuth.Existing): Existing = userAuth.transformInto[Existing]
