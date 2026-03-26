package io.tyoras.cards.domain.game

import cats.Show
import cats.data.NonEmptyList
import io.chrisdavenport.fuuid.FUUID

import java.time.ZonedDateTime
import scala.util.control.NoStackTrace
import cats.syntax.all.*
import io.circe.{Decoder, Encoder}
import io.tyoras.cards.domain.game
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.domain.game.schnapsen.given

trait GameInput:
  def label: String
  def playerId: FUUID

type GameType = GameTyp[?, ?]
sealed abstract class GameTyp[S : Encoder, I <: GameInput : Decoder](val label: String, val minPlayers: Int, val maxPlayers: Int):
  type State = S
  type Input = I
  given Encoder[State] = Encoder[S]
  given Decoder[Input] = Decoder[I]

object GameTyp:
  case object Schnapsen extends GameTyp[schnapsen.model.GameState, schnapsen.model.SchnapsenInput]("schnapsen", 2, 2)
  case object War       extends GameTyp[war.model.GameState, war.model.WarInput]("war", 2, 52)
  given Encoder[GameType] = Encoder.encodeString.contramap(_.label)
  given Decoder[GameType] = Decoder.decodeString.map(_.trim.toLowerCase).emap {
    case Schnapsen.label => Schnapsen.asRight
    case War.label       => War.asRight
    case other           => s"Unknown game type $other".asLeft
  }

trait ActiveGame[F[_], State, Input <: GameInput]:
  def gameType: GameTyp[State, Input]
  def playerIds: F[NonEmptyList[FUUID]]
  def currentState: F[State]
  def submitInput(input: Input): F[State]

sealed abstract class Game[State] extends Product with Serializable:
  protected type ThisType <: Game[State]

  def gameType: GameTyp[State, ?]
  def players: NonEmptyList[FUUID]
  def state: State
  def withUpdatedState(newState: State, updateDate: ZonedDateTime): ThisType

object Game:
  final case class Existing[State](id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data[State]) extends Game[State]:
    override protected type ThisType = Existing[State]

    override def gameType: GameTyp[State, ?]  = data.gameType
    override def players: NonEmptyList[FUUID] = data.players
    override def state: State                 = data.state

    override def withUpdatedState(newState: State, updateDate: ZonedDateTime): ThisType =
      copy(data = data.withUpdatedState(newState, updateDate), updatedAt = updateDate)

  object Existing:
    given [State]: Show[Existing[State]] = e => s"id = ${e.id} | created_at = ${e.createdAt} | updated_at = ${e.updatedAt} | ${e.data.show}"

  final case class Data[State](gameType: GameTyp[State, ?], players: NonEmptyList[FUUID], state: State) extends Game[State]:
    override protected type ThisType = Data[State]

    override def withUpdatedState(newState: State, updateDate: ZonedDateTime): ThisType = copy(state = newState)

  object Data:
    given [State]: Show[Data[State]] = d => s"""game = ${d.gameType} | players = ${d.players.toList.mkString(", ")} | State = ${d.state}"""

abstract class GameError(val code: String, msg: String) extends Exception(msg) with NoStackTrace
object GameError:
  case object NoPlayersError extends GameError("no_players", "Game without any players")
