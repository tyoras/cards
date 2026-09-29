package io.tyoras.cards.persistence.game.stats

import cats.effect.{Clock, Resource, Sync}
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.chrisdavenport.cats.effect.time.implicits.*
import io.tyoras.cards.domain.game.stats.GameStatRepository
import io.tyoras.cards.domain.game.stats.model.PlayerGameStat
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.domain.game.GameType
import io.tyoras.cards.persistence.PersistenceError
import skunk.*
import fs2.Stream
import org.typelevel.log4cats.LoggerFactory

object PostgresGameStatRepository:
  def of[F[_] : Sync : LoggerFactory](sessionPool: Resource[F, Session[F]]): F[GameStatRepository[F]] = Sync[F].delay {
    new GameStatRepository[F] {
      private val logger = LoggerFactory.getLogger

      override def writeMany(stats: NonEmptyList[PlayerGameStat]): F[NonEmptyList[PlayerGameStat.Existing]] = stats.traverse {
        case data: PlayerGameStat.Data     => insert(data)
        case stat: PlayerGameStat.Existing => updateOne(stat)
      }

      override def insert(data: PlayerGameStat.Data, withId: Option[PlayerGameStat.ID]): F[PlayerGameStat.Existing] =
        sessionPool.use { session =>
          val daoData = GameStatDAO.Data.fromDomain(data)
          withId
            .fold(session.prepareR(Statements.Insert.one).use(_.unique(daoData))) { id =>
              session.prepareR(Statements.Insert.oneWithId).use(_.unique(id -> daoData))
            }
            .flatMap(_.toDomainF)
            .onError(logger.error(_)(s"Failed to insert game stat ${withId.getOrElse("no id provided")}"))
            .adaptErr {
              case SqlState.UniqueViolation(ex) =>
                PersistenceError("already_exist", "Game stat already exist")
              case _ =>
                PersistenceError("unexpected_error", "Failed to insert game stat")
            }
        }

      private def updateOne(stat: PlayerGameStat.Existing): F[PlayerGameStat.Existing] =
        sessionPool
          .use { session =>
            for
              now     <- Clock[F].getZonedDateTimeUTC
              updated <- session.prepareR(Statements.Update.one).use(_.unique(GameStatDAO.Existing.fromDomain(stat) -> now))
              result  <- Sync[F].fromEither(updated.toDomain)
            yield result
          }
          .onError(logger.error(_)(s"Failed to update game stat ${stat.id}"))
          .adaptErr(_ => PersistenceError("unexpected_error", "Failed to update game stat"))

      override def readManyById(ids: NonEmptyList[PlayerGameStat.ID]): F[List[PlayerGameStat.Existing]] =
        sessionPool
          .use(_.prepareR(Statements.Select.many(ids.size)).use(_.stream(ids.toList, chunkSize).evalMap(_.toDomainF[F]).compile.toList))
          .onError(logger.error(_)(s"Failed to read many game stats by their ids"))
          .adaptErr(_ => PersistenceError("unexpected_error", "Failed to read game stats by id"))

      override def readManyByPlayer(playerId: User.ID): F[List[PlayerGameStat.Existing]] = {
        val playerIds = List(playerId)
        sessionPool.use(_.prepareR(Statements.Select.manyByPlayerId(playerIds.size)).use(_.stream(playerIds, chunkSize).evalMap(_.toDomainF[F]).compile.toList))
      }.onError(logger.error(_)(s"Failed to read many game stats by player $playerId"))
        .adaptErr(_ => PersistenceError("unexpected_error", s"Failed to read game stats by player $playerId"))

      override def readOne(playerId: User.ID, game: GameType): F[Option[PlayerGameStat.Existing]] =
        sessionPool
          .use(_.prepareR(Statements.Select.one).use(_.option(playerId -> game).flatMap(_.traverse(_.toDomainF[F]))))
          .onError(logger.error(_)(s"Failed to read game stat for player $playerId and game $game"))
          .adaptErr(_ => PersistenceError("unexpected_error", s"Failed to read game stat for player $playerId and game $game"))

      override def readAll: Stream[F, PlayerGameStat.Existing] =
        (for
          session  <- Stream.resource(sessionPool)
          prepared <- Stream.resource(session.prepareR(Statements.Select.all))
          results  <- prepared.stream(Void, chunkSize).evalMap(_.toDomainF)
        yield results)
          .onError(e => Stream.eval(logger.error(e)("Failed to read all game stats")))
          .adaptErr(_ => PersistenceError("unexpected_error", "Failed to read all game stats"))

      override def deleteMany(users: NonEmptyList[PlayerGameStat.Existing]): F[Unit] =
        sessionPool
          .use(_.prepareR(Statements.Delete.many(users.size)).use(_.execute(users.toList.map(_.id)).void))
          .onError(logger.error(_)(s"Failed to delete many game stats ${users.map(_.id).toList}"))
          .adaptErr(_ => PersistenceError("unexpected_error", "Failed to delete game stats"))

      override def deleteAll: F[Unit] = sessionPool
        .use(_.execute(Statements.Delete.all).void)
        .onError(logger.error(_)(s"Failed to delete all game stats"))
        .adaptErr(_ => PersistenceError("unexpected_error", "Failed to delete all game stats"))
    }
  }

  private val chunkSize = 1024
