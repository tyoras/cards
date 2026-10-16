package io.tyoras.cards.domain.game

import io.circe.{Decoder, Encoder}
import fs2.Stream
import io.tyoras.cards.domain.user.model.User

trait GameRepository[F[_]]:

  def insert[State : Decoder : Encoder](data: Game.Data[State], withId: Option[Game.ID] = None): F[Game.Existing[State]]

  def update[State : Decoder : Encoder](game: Game.Existing[State]): F[Game.Existing[State]]

  def readAll[State : Decoder](finished: Boolean): Stream[F, Game.Existing[State]]

  def readManyById[State : Decoder](ids: List[Game.ID]): F[List[Game.Existing[State]]]

  def readManyByUser[State : Decoder](userId: User.ID, finished: Boolean): F[List[Game.Existing[State]]]

  def deleteMany(game: List[Game.Existing[?]]): F[Unit]

  def deleteAll: F[Unit]
