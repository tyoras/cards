package io.tyoras.cards.persistence.auth

import cats.effect.Sync
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.auth.model.UserAuth
import io.tyoras.cards.persistence.ParsingError
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.partial.syntax.*
import io.tyoras.cards.persistence.codecs.skunk.{fuuid, timestampTZ}
import skunk.*
import skunk.codec.all.*
import io.tyoras.cards.persistence.auth.UserAuthDAO.Data.given

import java.time.ZonedDateTime

object UserAuthDAO:

  case class Data(userId: FUUID, passwordHash: Array[Byte], passwordSalt: Array[Byte]):
    lazy val toDomain: Either[ParsingError, UserAuth.Data] =
      this.transformIntoPartial[UserAuth.Data].asEither.left.map(e => ParsingError.InvalidCombination(s"Error while parsing UserAuth.Data: ${e.errors}"))

  object Data:
    given PartialTransformer[Data, UserAuth.Data] =
      PartialTransformer
        .define[Data, UserAuth.Data]
        .withFieldComputedPartial(_.passwordHash, h => UserAuth.PasswordHashBytes.either(h.passwordHash).asResult)
        .withFieldComputedPartial(_.passwordSalt, s => UserAuth.PasswordSaltBytes.either(s.passwordSalt).asResult)
        .buildTransformer
    val codec: Codec[Data]                        = (fuuid *: bytea *: bytea).to[Data]
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
