package io.tyoras.cards.domain.game.ers.model

import cats.data.NonEmptyList
import io.circe.derivation.*
import io.circe.Codec
import io.tyoras.cards.domain.card.{Card, Turn}
import io.tyoras.cards.domain.game.GameStateFilter
import io.tyoras.cards.domain.game.ers.codecs.given
import io.tyoras.cards.domain.game.ers.model.ERsGameState.SlapPenalty
import io.tyoras.cards.domain.game.ers.model.{Elimination, ERsGameState, PlayerId}
import io.tyoras.cards.domain.game.ers.penalty

sealed trait PlayerGameState:
  val code: String
  val label: String
  val turn: Turn

  /** The ID of the player seeing the states */
  val playerId: PlayerId

object PlayerGameState:
  final case class Init(override val turn: Turn, override val playerId: PlayerId, ready: Set[PlayerId], notReady: Set[PlayerId]) extends PlayerGameState:
    override val code: String  = "init"
    override val label: String = "Initialisation"

  final case class PlayerTurn(
      override val turn: Turn,
      override val playerId: PlayerId,
      waitedPlayer: PlayerId,
      pileTopCard: Option[Card],
      pileSize: Card.Count,
      handSize: Card.Count,
      opponents: Map[PlayerId, Card.Count]
  ) extends PlayerGameState:
    override val code: String  = "player-turn"
    override val label: String = s"Waiting for player [$waitedPlayer] to play"

  final case class ChallengeTurn(
      override val turn: Turn,
      override val playerId: PlayerId,
      owner: PlayerId,
      challenger: PlayerId,
      totalTries: Int,
      remainingTries: Int,
      pileTopCard: Card,
      pileSize: Card.Count,
      handSize: Card.Count,
      opponents: Map[PlayerId, Card.Count]
  ) extends PlayerGameState:
    override val code: String  = "challenge-turn"
    override val label: String = s"Challenge turn for player [$owner] against player [$challenger] | Tries ${totalTries - remainingTries}/$totalTries"

  final case class SlapPenalty(
      override val turn: Turn,
      override val playerId: PlayerId,
      penalizedPlayer: PlayerId,
      pileTopCard: Card,
      pileSize: Card.Count,
      handSize: Card.Count,
      opponents: Map[PlayerId, Card.Count],
      discardedCards: List[Card],
      remainingCardsToDiscard: Card.Count,
      acked: Set[PlayerId],
      notAcked: Set[PlayerId]
  ) extends PlayerGameState:
    override val code: String  = "slap-penalty"
    override val label: String = s"Player [$penalizedPlayer] has made an invalid slap"

  final case class PlayerWinTurn(
      override val turn: Turn,
      override val playerId: PlayerId,
      handSize: Card.Count,
      opponents: Map[PlayerId, Card.Count],
      winnerId: PlayerId,
      wonCards: NonEmptyList[Card],
      eliminated: Set[PlayerId],
      acked: Set[PlayerId],
      notAcked: Set[PlayerId]
  ) extends PlayerGameState:
    override val code: String  = "player-win-turn"
    override val label: String = s"Player $winnerId has won the turn and won ${wonCards.size} cards."

  final case class Finish(override val turn: Turn, override val playerId: PlayerId, winnerId: PlayerId, eliminations: List[Elimination])
      extends PlayerGameState:
    override val code: String  = "finish"
    override val label: String = "Finish"

  final case class Exit(override val turn: Turn, override val playerId: PlayerId) extends PlayerGameState:
    override val code: String  = "exit"
    override val label: String = "Exit"

  given GameStateFilter[ERsGameState] with
    type PlayerState = PlayerGameState
    override def codec: Codec[PlayerGameState] = summon[ConfiguredCodec[PlayerGameState]]
    extension (gameState: ERsGameState)
      def filterForPlayer(playerId: PlayerId): PlayerGameState =
        gameState match
          case s: ERsGameState.Init       => Init(s.context.turnNumber, playerId, s.ready, s.notReady)
          case s: ERsGameState.PlayerTurn =>
            PlayerTurn(
              turn = s.context.turnNumber,
              playerId = playerId,
              waitedPlayer = s.player,
              pileTopCard = s.pile.headOption,
              pileSize = Card.Count.assume(s.pile.size),
              handSize = Card.Count.assume(s.context.playerHand(playerId).size),
              opponents = s.context.playersCardCount
            )
          case s: ERsGameState.ChallengeTurn =>
            ChallengeTurn(
              turn = s.context.turnNumber,
              playerId = playerId,
              owner = s.owner,
              challenger = s.challenger,
              totalTries = s.totalTries,
              remainingTries = s.remainingTries,
              pileTopCard = s.pile.head,
              pileSize = Card.Count.assume(s.pile.size),
              handSize = Card.Count.assume(s.context.playerHand(playerId).size),
              opponents = s.context.playersCardCount
            )
          case s: ERsGameState.SlapPenalty =>
            SlapPenalty(
              turn = s.context.turnNumber,
              playerId = playerId,
              penalizedPlayer = s.player,
              pileTopCard = s.pile.head,
              pileSize = Card.Count.assume(s.pile.size),
              handSize = Card.Count.assume(s.context.playerHand(playerId).size),
              opponents = s.context.playersCardCount,
              discardedCards = s.discardedCards,
              remainingCardsToDiscard = Card.Count.assume(penalty - s.discardedCards.size),
              acked = s.acked,
              notAcked = s.notAcked
            )
          case s: ERsGameState.PlayerWinTurn =>
            PlayerWinTurn(
              s.context.turnNumber,
              playerId,
              Card.Count.assume(s.context.playerHand(playerId).size),
              s.context.playersCardCount,
              s.winnerId,
              s.wonCards,
              s.eliminated,
              s.acked,
              s.notAcked
            )
          case s: ERsGameState.Finish => Finish(s.context.turnNumber, playerId, s.winnerId, s.context.eliminations)
          case s: ERsGameState.Exit   => Exit(s.context.turnNumber, playerId)
