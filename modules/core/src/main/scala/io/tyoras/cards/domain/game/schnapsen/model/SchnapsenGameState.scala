package io.tyoras.cards.domain.game.schnapsen.model

import io.tyoras.cards.domain.card.Rank.Jack
import io.tyoras.cards.domain.game.schnapsen.{PlayerId, baseDeck, findPossibleMarriages}
import io.tyoras.cards.domain.card.{Card, Hand}
import io.tyoras.cards.domain.game.GameState
import io.tyoras.cards.domain.game.schnapsen.model.Role.*

sealed trait SchnapsenGameState extends GameState:
  def round: GameRound
  def playableCards: Hand = Nil
  override def toString   = s"$label$round\n\tPlayable cards \t${if playableCards.isEmpty then "None" else playableCards.mkString(" ")}"
  def player(id: PlayerId): Either[SchnapsenError, PlayerInfo] = round.context.player(id)

sealed trait PlayerTurn extends SchnapsenGameState:
  def currentPlayer: Player

sealed trait ForehandTurn extends PlayerTurn:
  def ongoingMarriage: Option[Marriage]
  lazy val possibleMarriages: List[Marriage] = findPossibleMarriages(currentPlayer.hand, round.trumpSuit)

sealed trait DealerTurn extends PlayerTurn:
  def forehandCard: Card

case class Init(round: GameRound) extends SchnapsenGameState:
  override val code: String  = "init"
  override val label: String = "Initialisation"

sealed abstract class EarlyGame(game: GameRound, currentRole: Role) extends PlayerTurn:
  val currentPlayer: Player = game.roles(currentRole)
  def playableCards: Hand

case class EarlyGameForehandTurn(round: GameRound, ongoingMarriage: Option[Marriage] = None) extends EarlyGame(round, Forehand) with ForehandTurn:
  override val code: String  = "early-game-forehand-turn"
  override val label: String = "Early game - forehand turn"
  lazy val trumpJack: Card   =
    baseDeck.find(c => c.suit == round.trumpSuit && c.rank.isInstanceOf[Jack]).getOrElse(throw new IllegalArgumentException("Trump jack not found"))
  lazy val canExchangeTrumpJack: Boolean = currentPlayer.hand.contains(trumpJack)
  override lazy val playableCards: Hand  = ongoingMarriage.fold(currentPlayer.hand)(m => List(m.king, m.queen))

case class EarlyGameDealerTurn(round: GameRound, forehandCard: Card) extends EarlyGame(round, Dealer) with DealerTurn:
  override val code: String        = "early-game-dealer-turn"
  override val label: String       = "Early game - dealer turn"
  override def toString            = s"${super.toString}\n\tForehand card\t$forehandCard"
  override val playableCards: Hand = currentPlayer.hand

sealed abstract class LateGame(game: GameRound, currentRole: Role) extends PlayerTurn:
  val currentPlayer: Player = game.roles(currentRole)

case class LateGameForehandTurn(round: GameRound, ongoingMarriage: Option[Marriage] = None) extends LateGame(round, Forehand) with ForehandTurn:
  override val code: String             = "late-game-forehand-turn"
  override val label: String            = "Late game - forehand turn"
  override lazy val playableCards: Hand = ongoingMarriage.fold(currentPlayer.hand)(m => List(m.king, m.queen))

case class LateGameDealerTurn(round: GameRound, forehandCard: Card) extends LateGame(round, Dealer) with DealerTurn:
  override val code: String             = "late-game-dealer-turn"
  override val label: String            = "Late game - dealer turn"
  override lazy val playableCards: Hand =
    currentPlayer.hand.filter(_.suit == forehandCard.suit) match
      case Nil =>
        currentPlayer.hand.filter(_.suit == round.trumpSuit) match
          case Nil            => currentPlayer.hand
          case trumpSuitCards => trumpSuitCards
      case sameSuit =>
        sameSuit.filter(_ > forehandCard) match
          case Nil         => sameSuit
          case higherCards => higherCards
  override def toString = s"${super.toString}\n\tForehand card\t$forehandCard"

case class Finish(round: GameRound, outcome: RoundOutcome) extends SchnapsenGameState:
  override val code: String  = "finish"
  override val label: String = "Finish"

case class Exit(round: GameRound) extends SchnapsenGameState:
  override val code: String  = "exit"
  override val label: String = "Exit"
