package io.tyoras.cards.persistence.game.stats

import cats.effect.Sync
import io.github.iltotore.iron.chimney.given
import io.tyoras.cards.domain.game.GameType
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.domain.game.stats.model.PlayerGameStat
import io.tyoras.cards.persistence.ParsingError
import skunk.Codec
import io.tyoras.cards.persistence.codecs.skunk.{gameType, timestampTZ}
import skunk.codec.all.*
import io.scalaland.chimney.PartialTransformer
import io.github.iltotore.iron.*
import io.scalaland.chimney.dsl.*

import scala.util.chaining.*
import java.time.ZonedDateTime
import java.util.UUID
import io.scalaland.chimney.Transformer

object GameStatDAO:

  case class Data(playerId: UUID, game: GameType, won: Int, draw: Int, lost: Int):
    lazy val toDomain: Either[ParsingError, PlayerGameStat.Data] =
      this
        .transformIntoPartial[PlayerGameStat.Data]
        .asEither
        .left
        .map(e => ParsingError.InvalidCombination(s"Error while parsing PlayerGameStat.Data: ${e.errors}"))

  object Data:
    given PartialTransformer[Data, PlayerGameStat.Data] =
      PartialTransformer.define[Data, PlayerGameStat.Data].withFieldComputed(_.playerId, _.playerId.pipe(User.ID(_))).buildTransformer
    given Transformer[PlayerGameStat.Data, Data] =
      Transformer.define[PlayerGameStat.Data, Data].withFieldComputed(_.playerId, _.playerId.unwrap).buildTransformer
    val codec: Codec[Data]                          = (uuid *: gameType *: int4 *: int4 *: int4).to[Data]
    def fromDomain(stat: PlayerGameStat.Data): Data = stat.transformInto[Data]

  case class Existing(id: UUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data):
    lazy val toDomain: Either[ParsingError, PlayerGameStat.Existing] =
      this
        .intoPartial[PlayerGameStat.Existing]
        .withFieldComputed(_.id, _.id.pipe(PlayerGameStat.ID(_)))
        .transform
        .asEither
        .left
        .map(e => ParsingError.InvalidCombination(s"Error while parsing PlayerGameStat.Existing: ${e.errors}"))

    def toDomainF[F[_] : Sync]: F[PlayerGameStat.Existing] = Sync[F].fromEither(this.toDomain)

  object Existing:
    val codec: Codec[Existing]                              = (uuid *: timestampTZ *: timestampTZ *: Data.codec).to[Existing]
    def fromDomain(stat: PlayerGameStat.Existing): Existing = stat.into[Existing].withFieldComputed(_.id, _.id.unwrap).transform
