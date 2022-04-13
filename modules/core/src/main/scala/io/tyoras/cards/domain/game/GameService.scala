package io.tyoras.cards.domain.game

import cats.Functor
import io.chrisdavenport.fuuid.FUUID
import io.circe.{Decoder, Encoder}
import cats.syntax.all.*

trait GameService[F[_]]:
  def create[State : Encoder : Decoder](game: Game.Data[State], withId: Option[FUUID] = None): F[Game.Existing[State]]

  def update[State : Decoder : Encoder](game: Game.Existing[State]): F[Game.Existing[State]]

  def readById[State : Decoder](id: FUUID): F[Option[Game.Existing[State]]]

  def readManyByUser[State : Decoder](userId: FUUID): F[List[Game.Existing[State]]]

  def readAll[State : Decoder]: F[List[Game.Existing[State]]]

  def delete(game: Game.Existing[_]): F[Unit] =
    deleteMany(List(game))

  def deleteMany(games: List[Game.Existing[_]]): F[Unit]

  val deleteAll: F[Unit]

object GameService:
  def of[F[_] : Functor](gameRepo: GameRepository[F]): GameService[F] = new:
    override def create[State : Encoder : Decoder](game: Game.Data[State], withId: Option[FUUID]): F[Game.Existing[State]] =
      gameRepo.insert(game, withId)

    override def update[State : Decoder : Encoder](game: Game.Existing[State]): F[Game.Existing[State]] =
      gameRepo.update(game)

    override def readById[State : Decoder](id: FUUID): F[Option[Game.Existing[State]]] =
      gameRepo.readManyById(List(id)).map(_.headOption)

    override def readManyByUser[State : Decoder](userId: FUUID): F[List[Game.Existing[State]]] =
      gameRepo.readManyByUser(userId)

    override def readAll[State : Decoder]: F[List[Game.Existing[State]]] =
      gameRepo.readAll

    override def deleteMany(games: List[Game.Existing[_]]): F[Unit] =
      gameRepo.deleteMany(games)

    override val deleteAll: F[Unit] =
      gameRepo.deleteAll
