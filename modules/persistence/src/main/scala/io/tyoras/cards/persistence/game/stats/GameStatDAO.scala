package io.tyoras.cards.persistence.game.stats

import cats.effect.Sync
import io.chrisdavenport.fuuid.FUUID
import io.github.iltotore.iron.chimney.given
import io.tyoras.cards.domain.game.GameType
import io.tyoras.cards.domain.game.stats.model.PlayerGameStat
import io.tyoras.cards.persistence.ParsingError
import skunk.Codec
import io.tyoras.cards.persistence.codecs.skunk.{fuuid, gameType, timestampTZ}
import skunk.codec.all.*
import io.scalaland.chimney.PartialTransformer
import io.github.iltotore.iron.*
import io.scalaland.chimney.dsl.*

import java.time.ZonedDateTime

object GameStatDAO:

  case class Data(playerId: FUUID, game: GameType, won: Int, draw: Int, lost: Int):
    lazy val toDomain: Either[ParsingError, PlayerGameStat.Data] =
      this
        .transformIntoPartial[PlayerGameStat.Data]
        .asEither
        .left
        .map(e => ParsingError.InvalidCombination(s"Error while parsing PlayerGameStat.Data: ${e.errors}"))

  object Data:
    val codec: Codec[Data]                          = (fuuid *: gameType *: int4 *: int4 *: int4).to[Data]
    def fromDomain(stat: PlayerGameStat.Data): Data = stat.transformInto[Data]

  case class Existing(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data):
    lazy val toDomain: Either[ParsingError, PlayerGameStat.Existing] =
      this
        .transformIntoPartial[PlayerGameStat.Existing]
        .asEither
        .left
        .map(e => ParsingError.InvalidCombination(s"Error while parsing PlayerGameStat.Existing: ${e.errors}"))

    def toDomainF[F[_] : Sync]: F[PlayerGameStat.Existing] = Sync[F].fromEither(this.toDomain)

  object Existing:
    val codec: Codec[Existing]                              = (fuuid *: timestampTZ *: timestampTZ *: Data.codec).to[Existing]
    def fromDomain(stat: PlayerGameStat.Existing): Existing = stat.transformInto[Existing]
