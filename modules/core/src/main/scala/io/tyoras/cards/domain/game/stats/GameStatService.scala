package io.tyoras.cards.domain.game.stats

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.tyoras.cards.domain.game.stats.model.{PlayerGameStat, PlayerGameStats}
import io.tyoras.cards.domain.user.model.User
import fs2.Stream
import io.tyoras.cards.domain.game.GameType

trait GameStatService[F[_]]:
  def updatePlayersStats(game: GameType, winners: List[User.ID], draws: List[User.ID], losers: List[User.ID]): F[NonEmptyList[PlayerGameStat.Existing]]
  def getPlayerStat(playerId: User.ID, game: GameType): F[PlayerGameStat]
  def getPlayerStats(playerId: User.ID): F[Option[PlayerGameStats]]

  def readAll: Stream[F, PlayerGameStat.Existing]

  def delete(stat: PlayerGameStat.Existing): F[Unit]

  def deleteMany(stats: NonEmptyList[PlayerGameStat.Existing]): F[Unit]

  def deleteAll: F[Unit]

object GameStatService:
  def of[F[_] : MonadThrow](gameStatRepo: GameStatRepository[F]): GameStatService[F] = new:
    override def updatePlayersStats(
        game: GameType,
        winners: List[User.ID],
        draws: List[User.ID],
        losers: List[User.ID]
    ): F[NonEmptyList[PlayerGameStat.Existing]] =
      for
        winnersStats <- getPlayersStats(winners, game, _.incrementWon)
        drawStats    <- getPlayersStats(draws, game, _.incrementDraw)
        losersStats  <- getPlayersStats(losers, game, _.incrementLost)
        allStats     <- MonadThrow[F]
          .fromOption(NonEmptyList.fromList(winnersStats ++ drawStats ++ losersStats), new IllegalArgumentException("No player stats to update"))
        savedStats <- gameStatRepo.writeMany(allStats)
      yield savedStats

    private def getPlayersStats(playersIds: List[User.ID], game: GameType, update: PlayerGameStat => PlayerGameStat): F[List[PlayerGameStat]] =
      playersIds.traverse { playerId =>
        gameStatRepo.readOne(playerId, game).map(_.getOrElse(PlayerGameStat.Data.apply(playerId, game))).map(update)
      }

    override def getPlayerStat(playerId: User.ID, gameType: GameType): F[PlayerGameStat] =
      gameStatRepo.readOne(playerId, gameType).map {
        _.getOrElse(PlayerGameStat.Data(playerId, gameType))
      }

    override def getPlayerStats(playerId: User.ID): F[Option[PlayerGameStats]] =
      gameStatRepo.readManyByPlayer(playerId).map {
        NonEmptyList.fromList(_).map(PlayerGameStats(playerId, _))
      }

    override val readAll: Stream[F, PlayerGameStat.Existing] =
      gameStatRepo.readAll

    override def delete(stat: PlayerGameStat.Existing): F[Unit] =
      deleteMany(NonEmptyList.one(stat))

    override def deleteMany(stats: NonEmptyList[PlayerGameStat.Existing]): F[Unit] =
      gameStatRepo.deleteMany(stats)

    override val deleteAll: F[Unit] =
      gameStatRepo.deleteAll
