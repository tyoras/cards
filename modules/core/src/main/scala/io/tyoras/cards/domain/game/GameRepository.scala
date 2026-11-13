package io.tyoras.cards.domain.game

import io.circe.{Codec, Decoder}
import fs2.Stream
import io.tyoras.cards.domain.user.model.User

trait GameRepository[F[_]]:

  def insert[State <: GameState : Codec](data: Game.Data[State], withId: Option[Game.ID] = None): F[Game.Existing[State]]

  def update[State <: GameState : Codec](game: Game.Existing[State]): F[Game.Existing[State]]

  def readAll[State <: GameState : Decoder](finished: Boolean): Stream[F, Game.Existing[State]]

  def readManyById[State <: GameState : Decoder](ids: List[Game.ID]): F[List[Game.Existing[State]]]

  def readManyByUser[State <: GameState : Decoder](userId: User.ID, finished: Boolean): F[List[Game.Existing[State]]]

  def deleteMany(game: List[Game.Existing[?]]): F[Unit]

  def deleteAll: F[Unit]
