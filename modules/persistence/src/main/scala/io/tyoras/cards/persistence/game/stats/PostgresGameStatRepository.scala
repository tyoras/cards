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

object PostgresGameStatRepository:
  def of[F[_] : Sync](sessionPool: Resource[F, Session[F]]): F[GameStatRepository[F]] = Sync[F].delay {
    new GameStatRepository[F] {

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
            .adaptErr { case SqlState.UniqueViolation(ex) =>
              PersistenceError("already_exist", "Game stat already exist")
            }
        }

      private def updateOne(stat: PlayerGameStat.Existing): F[PlayerGameStat.Existing] =
        sessionPool.use { session =>
          for
            now     <- Clock[F].getZonedDateTimeUTC
            updated <- session.prepareR(Statements.Update.one).use(_.unique(GameStatDAO.Existing.fromDomain(stat) -> now))
            result  <- Sync[F].fromEither(updated.toDomain)
          yield result
        }

      override def readManyById(ids: NonEmptyList[PlayerGameStat.ID]): F[List[PlayerGameStat.Existing]] =
        sessionPool.use(_.prepareR(Statements.Select.many(ids.size)).use(_.stream(ids.toList, chunkSize).evalMap(_.toDomainF[F]).compile.toList))

      override def readManyByPlayer(playerId: User.ID): F[List[PlayerGameStat.Existing]] = {
        val playerIds = List(playerId)
        sessionPool.use(_.prepareR(Statements.Select.manyByPlayerId(playerIds.size)).use(_.stream(playerIds, chunkSize).evalMap(_.toDomainF[F]).compile.toList))
      }

      override def readOne(playerId: User.ID, game: GameType): F[Option[PlayerGameStat.Existing]] =
        sessionPool.use(_.prepareR(Statements.Select.one).use(_.option(playerId -> game).flatMap(_.traverse(_.toDomainF[F]))))

      override def readAll: Stream[F, PlayerGameStat.Existing] =
        for
          session  <- Stream.resource(sessionPool)
          prepared <- Stream.resource(session.prepareR(Statements.Select.all))
          results  <- prepared.stream(Void, chunkSize).evalMap(_.toDomainF)
        yield results

      override def deleteMany(users: NonEmptyList[PlayerGameStat.Existing]): F[Unit] =
        sessionPool.use(_.prepareR(Statements.Delete.many(users.size)).use(_.execute(users.toList.map(_.id)).void))

      override def deleteAll: F[Unit] = sessionPool.use(_.execute(Statements.Delete.all).void)
    }
  }

  private val chunkSize = 1024
