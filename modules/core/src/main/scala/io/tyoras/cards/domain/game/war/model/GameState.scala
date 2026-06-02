package io.tyoras.cards.domain.game.war.model

import cats.data.{NonEmptyList, NonEmptySet}
import io.tyoras.cards.domain.card.Card
import cats.syntax.all.*
import io.tyoras.cards.domain.game.war.model.GameState.WarTurn.BattleRound

sealed trait GameState:
  def code: String
  def label: String
  def context: GameContext

object GameState:
  final case class Init(override val context: GameContext, ready: Set[PlayerId] = Set.empty) extends GameState:
    override val code: String        = "init"
    override val label: String       = "Initialisation"
    lazy val notReady: Set[PlayerId] = context.players.keySet.diff(ready)

  final case class BattleTurn(override val context: GameContext, playedCards: Map[PlayerId, Card] = Map.empty) extends GameState:
    override val code: String            = "battle-turn"
    lazy val missingPlays: Set[PlayerId] =
      // only expect cards from players who still have cards to plays
      context.players.filterNot(_._2.eliminated).keySet.diff(playedCards.keySet)

    override val label: String = s"Waiting for players [${missingPlays.mkString(", ")}] to play"

  final case class WarTurn(override val context: GameContext, battles: NonEmptyList[WarTurn.BattleRound]) extends GameState:
    override val code: String                              = "war-turn"
    val currentRound: BattleRound                          = battles.last
    private val currentRoundPlayers: NonEmptySet[PlayerId] = currentRound.involvedPlayers
    private val playersWithCardsInCurrentRound             = currentRoundPlayers.filter(id => context.player(id).exists(!_.eliminated))
    val missingHidden: Set[PlayerId]                       = playersWithCardsInCurrentRound.diff(currentRound.hiddenPlayedCards.keySet)
    val missingFighting: Set[PlayerId]                     = playersWithCardsInCurrentRound.diff(currentRound.fightingCards.keySet)
    val missingPlays: Set[PlayerId]                        = missingFighting.union(missingHidden)
    val allCardPlayed: Boolean                             = missingPlays.isEmpty
    val heap: Set[Card]                                    = battles.map(_.heap).reduce
    override val label: String                             = s"War battle round ${battles.size - 1} between players [${currentRoundPlayers.mkString_(", ")}]"

    def playCard(playerId: PlayerId, card: Card): WarTurn = {
      val updatedRound =
        if currentRound.hiddenPlayedCards.contains(playerId) then currentRound.copy(fightingCards = currentRound.fightingCards.updated(playerId, card))
        else currentRound.copy(hiddenPlayedCards = currentRound.hiddenPlayedCards.updated(playerId, card))
      val updatedBattles = NonEmptyList.fromListUnsafe(battles.init) :+ updatedRound
      copy(battles = updatedBattles)
    }

  object WarTurn:
    final case class BattleRound(involvedPlayers: NonEmptySet[PlayerId], hiddenPlayedCards: Map[PlayerId, Card], fightingCards: Map[PlayerId, Card]):
      val heap: Set[Card] = hiddenPlayedCards.values.toSet ++ fightingCards.values.toSet

  final case class PlayerWinTurn(
      override val context: GameContext,
      winnerId: PlayerId,
      wonCards: Set[Card],
      eliminated: Set[PlayerId],
      acked: Set[PlayerId] = Set.empty
  ) extends GameState:
    override val code: String        = "player-win-turn"
    override val label: String       = s"Player $winnerId has won the turn and won ${wonCards.size} cards."
    lazy val notAcked: Set[PlayerId] = context.players.keySet.diff(acked)

  final case class Finish(override val context: GameContext, winnerId: PlayerId) extends GameState:
    override val code: String  = "finish"
    override val label: String = "Finish"

  final case class Exit(override val context: GameContext) extends GameState:
    override val code: String  = "exit"
    override val label: String = "Exit"
