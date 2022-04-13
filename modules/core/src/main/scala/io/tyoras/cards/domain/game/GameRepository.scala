package io.tyoras.cards.domain.game

import io.chrisdavenport.fuuid.FUUID
import io.circe.{Decoder, Encoder}

trait GameRepository[F[_]]:

  def insert[State : Decoder : Encoder](data: Game.Data[State], withId: Option[FUUID] = None): F[Game.Existing[State]]

  def update[State : Decoder : Encoder](game: Game.Existing[State]): F[Game.Existing[State]]

  def readAll[State : Decoder]: F[List[Game.Existing[State]]]

  def readManyById[State : Decoder](ids: List[FUUID]): F[List[Game.Existing[State]]]

  def readManyByUser[State : Decoder](userId: FUUID): F[List[Game.Existing[State]]]

  def deleteMany(game: List[Game.Existing[_]]): F[Unit]

  def deleteAll: F[Unit]
