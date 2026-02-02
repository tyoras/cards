package io.tyoras.cards.persistence.game

import cats.{Eq, MonadThrow}
import cats.effect.{Clock, Resource, Sync}
import cats.syntax.all.*
import fs2.{Chunk, Pipe, Stream}
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.cats.effect.time.implicits.*
import io.circe.{Decoder, Encoder}
import io.tyoras.cards.domain.game.{Game, GameRepository}
import io.tyoras.cards.persistence.PersistenceError
import io.tyoras.cards.persistence.game.Statements
import skunk.Session

object PostgresGameRepository:
  extension [F[_], A : Eq, B](s: Stream[F, (A, B)])
    def chunkAdjacent: Stream[F, (A, Chunk[B])] =
      s.groupAdjacentBy(_._1).map { case (a, cab) =>
        a -> cab.collect(_._2)
      }

  def of[F[_] : Sync](sessionPool: Resource[F, Session[F]]): F[GameRepository[F]] = Sync[F].delay {
    new GameRepository[F]:
      override def insert[State : Decoder : Encoder](data: Game.Data[State], withId: Option[FUUID] = None): F[Game.Existing[State]] =
        sessionPool.use { session =>
          session.transaction.use { _ =>
            for
              inserted <- withId.fold(session.prepareR(Statements.Insert.one).use(_.unique(GameCreationDBModel.fromGameData[State](data)))) { id =>
                session.prepareR(Statements.Insert.oneWithId).use(_.unique(id -> GameCreationDBModel.fromGameData[State](data)))
              }
              _      <- data.players.traverse(playerId => session.prepareR(Statements.Insert.onePlayer).use(_.execute(inserted.id -> playerId)))
              result <- Sync[F].fromEither(inserted.toExistingGame[State](data.players))
            yield result
          }

        }

      override def update[State : Decoder : Encoder](game: Game.Existing[State]): F[Game.Existing[State]] =
        sessionPool.use { session =>
          for
            now     <- Clock[F].getZonedDateTimeUTC
            updated <- session.prepareR(Statements.Update.one).use(_.unique(GameUpdateDBModel.fromExisingGame(game, now)))
            result  <- Sync[F].fromEither(updated.toExistingGame[State](game.data.players))
          yield result
        }

      override def readAll[State : Decoder]: F[List[Game.Existing[State]]] = List.empty.pure
      // TODO implement read all
      // FIXME what can be passed to .stream(...) as Void as no instance ?
//        sessionPool.use(_.prepare(Statements.Select.all).use(_.stream(???, chunkSize).chunkAdjacent.through(toExisting[F, State]).compile.toList))

      override def readManyById[State : Decoder](ids: List[FUUID]): F[List[Game.Existing[State]]] =
        sessionPool.use(
          _.prepareR(Statements.Select.many(ids.size)).use(
            _.stream(ids, chunkSize).chunkAdjacent.through(toExisting[F, State]).compile.toList
          )
        )

      override def readManyByUser[State : Decoder](userId: FUUID): F[List[Game.Existing[State]]] =
        sessionPool.use(
          _.prepareR(Statements.Select.byUser).use(
            _.stream(userId, chunkSize).chunkAdjacent.through(toExisting[F, State]).compile.toList
          )
        )

      override def deleteMany(games: List[Game.Existing[?]]): F[Unit] =
        sessionPool.use(_.prepareR(Statements.Delete.many(games.size)).use(_.execute(games.map(_.id)).void))

      override def deleteAll: F[Unit] = sessionPool.use(_.execute(Statements.Delete.all).void)
  }

  private def toExisting[F[_] : MonadThrow, State : Decoder]: Pipe[F, (GameReadDBModel, Chunk[FUUID]), Game.Existing[State]] = _.evalMap {
    case (read, userIds) =>
      for
        players <- MonadThrow[F].fromOption(userIds.toNel, PersistenceError("invalid_game", "Read a game without players"))
        games   <- MonadThrow[F].fromEither(read.toExistingGame[State](players)).adaptError(e => PersistenceError("invalid_game", e.getMessage))
      yield games
  }

  private val chunkSize = 1024
