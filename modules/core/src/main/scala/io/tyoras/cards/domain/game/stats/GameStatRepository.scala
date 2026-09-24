package io.tyoras.cards.domain.game.stats

import cats.data.NonEmptyList
import io.tyoras.cards.domain.game.stats.model.PlayerGameStat
import io.tyoras.cards.domain.user.model.User
import fs2.Stream
import io.tyoras.cards.domain.game.GameType

trait GameStatRepository[F[_]]:
  def writeMany(stats: NonEmptyList[PlayerGameStat]): F[NonEmptyList[PlayerGameStat.Existing]]

  def insert(data: PlayerGameStat.Data, withId: Option[PlayerGameStat.ID] = None): F[PlayerGameStat.Existing]

  def readManyById(ids: NonEmptyList[PlayerGameStat.ID]): F[List[PlayerGameStat.Existing]]

  def readManyByPlayer(playerId: User.ID): F[List[PlayerGameStat.Existing]]

  def readOne(playerId: User.ID, game: GameType): F[Option[PlayerGameStat.Existing]]

  def readAll: Stream[F, PlayerGameStat.Existing]

  def deleteMany(stats: NonEmptyList[PlayerGameStat.Existing]): F[Unit]

  def deleteAll: F[Unit]
