package io.tyoras.cards.domain.game.ers.model

import cats.data.NonEmptyList
import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.game.ers.penalty

sealed trait GameState:
  def code: String
  def label: String
  def context: GameContext

object GameState:
  final case class Init(override val context: GameContext, ready: Set[PlayerId] = Set.empty) extends GameState:
    override val code: String        = "init"
    override val label: String       = "Initialisation"
    lazy val notReady: Set[PlayerId] = context.players.keySet.diff(ready)

  final case class PlayerTurn(override val context: GameContext, player: PlayerId, pile: List[Card] = List.empty) extends GameState:
    override val code: String = "player-turn"

    override val label: String = s"Waiting for player [$player] to play"

    lazy val nextPlayer: Option[PlayerId] = context.nextPlayerWithCards(player)

  final case class ChallengeTurn(
      override val context: GameContext,
      owner: PlayerId,
      challenger: PlayerId,
      totalTries: Int,
      remainingTries: Int,
      pile: NonEmptyList[Card]
  ) extends GameState:
    override val code: String  = "challenge-turn"
    override val label: String = s"Challenge turn for player [$owner] against player [$challenger] | Tries ${totalTries - remainingTries}/$totalTries"

  final case class SlapPenalty(
      override val context: GameContext,
      player: PlayerId,
      pile: NonEmptyList[Card],
      originalState: PlayerTurn | ChallengeTurn,
      discardedCards: List[Card] = List.empty,
      acked: Set[PlayerId] = Set.empty
  ) extends GameState:
    override val code: String        = "slap-penalty"
    override val label: String       = s"Player [$player] has made an invalid slap"
    lazy val notAcked: Set[PlayerId] = context.players.keySet.diff(acked) - player
    lazy val penaltyPayed: Boolean   = discardedCards.size == penalty || context.playerHand(player).isEmpty

  final case class PlayerWinTurn(
      override val context: GameContext,
      winnerId: PlayerId,
      wonCards: NonEmptyList[Card],
      eliminated: Set[PlayerId],
      acked: Set[PlayerId] = Set.empty
  ) extends GameState:
    override val code: String        = "player-win-turn"
    override val label: String       = s"Player $winnerId has won the turn and won ${wonCards.size} cards."
    lazy val notAcked: Set[PlayerId] = context.players.keySet.diff(acked)

  final case class Finish(override val context: GameContext, winnerId: PlayerId) extends GameState:
    override val code: String       = "finish"
    override val label: String      = "Finish"
    lazy val losers: List[PlayerId] = context.players.keySet.filterNot(_ == winnerId).toList

  final case class Exit(override val context: GameContext) extends GameState:
    override val code: String  = "exit"
    override val label: String = "Exit"
