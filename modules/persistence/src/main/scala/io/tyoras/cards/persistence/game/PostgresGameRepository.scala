package io.tyoras.cards.persistence.game

import cats.{Eq, MonadThrow}
import cats.effect.{Clock, Resource, Sync}
import cats.implicits.catsSyntaxApplicativeError
import cats.syntax.all.*
import fs2.{Chunk, Pipe, Stream}
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.cats.effect.time.implicits.*
import io.circe.{Decoder, Encoder}
import io.tyoras.cards.domain.game.{Game, GameRepository}
import io.tyoras.cards.persistence.PersistenceError
import io.tyoras.cards.persistence.game.Statements
import org.typelevel.log4cats.LoggerFactory
import skunk.*

object PostgresGameRepository:
  extension [F[_], A : Eq, B](s: Stream[F, (A, B)])
    def chunkAdjacent: Stream[F, (A, Chunk[B])] =
      s.groupAdjacentBy(_._1).map { case (a, cab) =>
        a -> cab.collect(_._2)
      }

  def of[F[_] : Sync : LoggerFactory](sessionPool: Resource[F, Session[F]]): F[GameRepository[F]] = Sync[F].delay {
    new GameRepository[F]:
      private val logger = LoggerFactory.getLogger

      override def insert[State : Decoder : Encoder](data: Game.Data[State], withId: Option[FUUID] = None): F[Game.Existing[State]] =
        sessionPool.use { session =>
          session.transaction.use { _ =>
            for
              inserted <- withId.fold(session.prepareR(Statements.Insert.one).use(_.unique(GameCreationDBModel.fromGameData[State](data)))) { id =>
                session
                  .prepareR(Statements.Insert.oneWithId)
                  .use(_.unique(id -> GameCreationDBModel.fromGameData[State](data)))
                  .onError(logger.error(_)(s"Failed to insert game $id"))
                  .adaptErr {
                    case SqlState.UniqueViolation(ex) =>
                      PersistenceError("already_exist", s"Game $id already exist")
                    case _ =>
                      PersistenceError("unexpected_error", s"Failed to insert game $id")
                  }
              }
              _ <- data.players
                .traverse(playerId => session.prepareR(Statements.Insert.onePlayer).use(_.execute(inserted.id -> playerId)))
                .onError(logger.error(_)(s"Failed to insert players for game ${inserted.id}"))
                .adaptErr(_ => PersistenceError("insert_players_failed", s"Failed to insert players for game ${inserted.id}"))
              result <- Sync[F].fromEither(inserted.toExistingGame[State](data.players))
            yield result
          }

        }

      override def update[State : Decoder : Encoder](game: Game.Existing[State]): F[Game.Existing[State]] =
        sessionPool.use { session =>
          for
            now     <- Clock[F].getZonedDateTimeUTC
            updated <- session
              .prepareR(Statements.Update.one)
              .use(_.unique(GameUpdateDBModel.fromExisingGame(game, now)))
              .onError(logger.error(_)(s"Failed to update game ${game.id}"))
              .adaptErr(_ => PersistenceError("update_failed", s"Failed to update game ${game.id}"))
            result <- Sync[F].fromEither(updated.toExistingGame[State](game.data.players))
          yield result
        }

      override def readAll[State : Decoder](finished: Boolean): Stream[F, Game.Existing[State]] =
        for
          session  <- Stream.resource(sessionPool)
          prepared <- Stream.resource(session.prepareR(Statements.Select.all(finished)))
          results  <- prepared
            .stream(Void, chunkSize)
            .chunkAdjacent
            .through(toExisting[F, State])
            .onError(e => Stream.eval(logger.error(e)(s"Failed to read all games")))
            .adaptErr(_ => PersistenceError("read_all_failed", s"Failed to read all games"))
        yield results

      override def readManyById[State : Decoder](ids: List[FUUID]): F[List[Game.Existing[State]]] =
        sessionPool
          .use(
            _.prepareR(Statements.Select.many(ids.size)).use(
              _.stream(ids, chunkSize).chunkAdjacent.through(toExisting[F, State]).compile.toList
            )
          )
          .onError(logger.error(_)(s"Failed to read games by ids $ids"))
          .adaptErr(_ => PersistenceError("read_many_failed", s"Failed to read games by ids $ids"))

      override def readManyByUser[State : Decoder](userId: FUUID, finished: Boolean): F[List[Game.Existing[State]]] =
        sessionPool
          .use(
            _.prepareR(Statements.Select.byUser(finished)).use(
              _.stream(userId, chunkSize).chunkAdjacent.through(toExisting[F, State]).compile.toList
            )
          )
          .onError(logger.error(_)(s"Failed to read games by user $userId"))
          .adaptErr(_ => PersistenceError("read_many_failed", s"Failed to read games by user $userId"))

      override def deleteMany(games: List[Game.Existing[?]]): F[Unit] =
        sessionPool
          .use(_.prepareR(Statements.Delete.many(games.size)).use(_.execute(games.map(_.id)).void))
          .onError(logger.error(_)(s"Failed to delete games ${games.map(_.id)}"))
          .adaptErr(_ => PersistenceError("delete_many_failed", s"Failed to delete games ${games.map(_.id)}"))
          .whenA(games.nonEmpty)

      override def deleteAll: F[Unit] = sessionPool
        .use(_.execute(Statements.Delete.all).void)
        .onError(logger.error(_)(s"Failed to delete all games"))
        .adaptErr(_ => PersistenceError("delete_all_failed", s"Failed to delete all games"))

  }

  private def toExisting[F[_] : MonadThrow, State : Decoder]: Pipe[F, (GameReadDBModel, Chunk[FUUID]), Game.Existing[State]] = _.evalMap {
    case (read, userIds) =>
      for
        players <- MonadThrow[F].fromOption(userIds.toNel, PersistenceError("invalid_game", "Read a game without players"))
        games   <- MonadThrow[F].fromEither(read.toExistingGame[State](players)).adaptError(e => PersistenceError("invalid_game", e.getMessage))
      yield games
  }

  private val chunkSize = 1024
