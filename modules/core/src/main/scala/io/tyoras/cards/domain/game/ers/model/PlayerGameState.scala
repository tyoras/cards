package io.tyoras.cards.domain.game.ers.model

import cats.data.NonEmptyList
import io.circe.derivation.*
import io.circe.Codec
import io.tyoras.cards.domain.card.{Card, Hand, Turn}
import io.tyoras.cards.domain.game.GameStateFilter
import io.tyoras.cards.domain.game.ers.codecs.given
import io.tyoras.cards.domain.game.ers.model.GameState.SlapPenalty
import io.tyoras.cards.domain.game.ers.model.{Elimination, GameState, PlayerId}

sealed trait PlayerGameState:
  val code: String
  val label: String
  val turn: Turn
  val playerId: PlayerId

object PlayerGameState:
  final case class Init(override val turn: Turn, override val playerId: PlayerId, ready: Set[PlayerId], notReady: Set[PlayerId]) extends PlayerGameState:
    override val code: String  = "init"
    override val label: String = "Initialisation"

  final case class PlayerWinTurn(
      override val turn: Turn,
      override val playerId: PlayerId,
      hand: Hand,
      opponents: Map[PlayerId, Card.Count],
      winnerId: PlayerId,
      wonCards: NonEmptyList[Card],
      eliminated: Set[PlayerId],
      acked: Set[PlayerId],
      notAcked: Set[PlayerId]
  ) extends PlayerGameState:
    override val code: String  = "player-win-turn"
    override val label: String = s"Player $winnerId has won the turn and won ${wonCards.size} cards."

  // TODO implement missing ERS player game states

  final case class Finish(override val turn: Turn, override val playerId: PlayerId, winnerId: PlayerId, eliminations: List[Elimination])
      extends PlayerGameState:
    override val code: String  = "finish"
    override val label: String = "Finish"

  final case class Exit(override val turn: Turn, override val playerId: PlayerId) extends PlayerGameState:
    override val code: String  = "exit"
    override val label: String = "Exit"

  given GameStateFilter[GameState] with
    type PlayerState = PlayerGameState
    override def codec: Codec[PlayerGameState] = summon[ConfiguredCodec[PlayerGameState]]
    extension (gameState: GameState)
      def filterForPlayer(playerId: PlayerId): PlayerGameState =
        gameState match
          case s: GameState.Init => Init(s.context.turnNumber, playerId, s.ready, s.notReady)
          // TODO handle missing ERS player game states
          case s: GameState.PlayerTurn    => ???
          case s: GameState.ChallengeTurn => ???
          case s: SlapPenalty             => ???
          case s: GameState.PlayerWinTurn =>
            PlayerWinTurn(
              s.context.turnNumber,
              playerId,
              s.context.playerHand(playerId),
              s.context.playersCardCount,
              s.winnerId,
              s.wonCards,
              s.eliminated,
              s.acked,
              s.notAcked
            )
          case s: GameState.Finish => Finish(s.context.turnNumber, playerId, s.winnerId, s.context.eliminations)
          case s: GameState.Exit   => Exit(s.context.turnNumber, playerId)
