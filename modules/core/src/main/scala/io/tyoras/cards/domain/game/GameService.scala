package io.tyoras.cards.domain.game

import cats.MonadThrow
import io.circe.{Codec, Decoder}
import cats.syntax.all.*
import fs2.Stream
import io.tyoras.cards.domain.game.GameError.{GameAlreadyFinished, GameNotFound}
import io.tyoras.cards.domain.user.model.User

import java.time.ZonedDateTime

trait GameService[F[_]]:
  def create[State : Codec](game: Game.Data[State], withId: Option[Game.ID] = None): F[Game.Existing[State]]

  def update[State : Codec](game: Game.Existing[State]): F[Game.Existing[State]]

  def finish[State : Codec](gameId: Game.ID, finalState: State, finishedAt: ZonedDateTime): F[Game.Existing[State]]

  def readById[State : Decoder](id: Game.ID): F[Option[Game.Existing[State]]]

  def readManyByUser[State : Decoder](userId: User.ID, finished: Boolean): F[List[Game.Existing[State]]]

  def readAll[State : Decoder](finished: Boolean): Stream[F, Game.Existing[State]]

  def delete(game: Game.Existing[?]): F[Unit] =
    deleteMany(List(game))

  def deleteMany(games: List[Game.Existing[?]]): F[Unit]

  val deleteAll: F[Unit]

object GameService:
  def of[F[_] : MonadThrow](gameRepo: GameRepository[F]): GameService[F] = new:
    override def create[State : Codec](game: Game.Data[State], withId: Option[Game.ID]): F[Game.Existing[State]] =
      gameRepo.insert(game, withId)

    override def update[State : Codec](game: Game.Existing[State]): F[Game.Existing[State]] =
      gameRepo.update(game)

    override def finish[State : Codec](gameId: Game.ID, finalState: State, finishedAt: ZonedDateTime): F[Game.Existing[State]] =
      for
        found <- readById[State](gameId)
        game  <- MonadThrow[F].fromOption(found, GameNotFound(gameId)).ensure(GameAlreadyFinished(gameId))(_.data.finishedAt.isEmpty)

        updatedGame <-
          val finishedGame = game.withUpdatedState(finalState).withFinishedAt(finishedAt)
          update(finishedGame)
      yield updatedGame

    override def readById[State : Decoder](id: Game.ID): F[Option[Game.Existing[State]]] =
      gameRepo.readManyById(List(id)).map(_.headOption)

    override def readManyByUser[State : Decoder](userId: User.ID, finished: Boolean): F[List[Game.Existing[State]]] =
      gameRepo.readManyByUser(userId, finished)

    override def readAll[State : Decoder](finished: Boolean): Stream[F, Game.Existing[State]] =
      gameRepo.readAll(finished)

    override def deleteMany(games: List[Game.Existing[?]]): F[Unit] =
      gameRepo.deleteMany(games)

    override val deleteAll: F[Unit] =
      gameRepo.deleteAll
