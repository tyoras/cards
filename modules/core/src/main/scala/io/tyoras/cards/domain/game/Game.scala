package io.tyoras.cards.domain.game

import cats.Show
import cats.data.NonEmptyList
import cats.implicits.toShow
import io.chrisdavenport.fuuid.FUUID

import java.time.ZonedDateTime

enum GameType:
  case Schnapsen, War

sealed abstract class Game[State] extends Product with Serializable:
  protected type ThisType <: Game[State]

  def gameType: GameType
  def players: NonEmptyList[FUUID]
  def state: State
  def withUpdatedState(newState: State, updateDate: ZonedDateTime): ThisType

object Game:
  final case class Existing[State](id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data[State]) extends Game[State]:
    override protected type ThisType = Existing[State]

    override def gameType: GameType = data.gameType
    override def players: NonEmptyList[FUUID] = data.players
    override def state: State = data.state

    override def withUpdatedState(newState: State, updateDate: ZonedDateTime): ThisType =
      copy(data = data.withUpdatedState(newState, updateDate), updatedAt = updateDate)

  object Existing:
    given [State]: Show[Existing[State]] = e => s"id = ${e.id} | created_at = ${e.createdAt} | updated_at = ${e.updatedAt} | ${e.data.show}"

  final case class Data[State](gameType: GameType, players: NonEmptyList[FUUID], state: State) extends Game[State]:
    override protected type ThisType = Data[State]

    override def withUpdatedState(newState: State, updateDate: ZonedDateTime): ThisType = copy(state = newState)

  object Data:
    given [State]: Show[Data[State]] = d => s"""game = ${d.gameType} | players = ${d.players.toList.mkString(", ")} | State = ${d.state}"""
