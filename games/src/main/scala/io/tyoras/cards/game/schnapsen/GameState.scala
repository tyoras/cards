package io.tyoras.cards.game.schnapsen

import io.tyoras.cards.game.schnapsen.model._
import io.tyoras.cards.{Card, Hand, Jack}

sealed trait GameState {
  def name: String
  def round: GameRound
  def playableCards: Hand = Nil
  override def toString = s"$name$round\n\tPlayable cards \t${if (playableCards.isEmpty) "None" else playableCards.mkString(" ")}"
  def player(id: PlayerId): Either[SchnapsenError, PlayerInfo] = round.context.player(id)
}

sealed trait PlayerTurn extends GameState {
  def currentPlayer: Player
}

sealed trait ForehandTurn extends PlayerTurn {
  lazy val possibleMarriages: List[Marriage] = findPossibleMarriages(currentPlayer.hand, round.trumpSuit)
}

sealed trait DealerTurn extends PlayerTurn {
  def forehandCard: Card
}

case class Init(round: GameRound) extends GameState {
  val name: String = "Initialisation"
}

sealed abstract class EarlyGame(game: GameRound, currentRole: Role) extends PlayerTurn {
  val currentPlayer: Player = game.roles(currentRole)
  def playableCards: Hand
}

case class EarlyGameForehandTurn(round: GameRound, ongoingMarriage: Option[Marriage] = None) extends EarlyGame(round, Forehand) with ForehandTurn {
  override val name: String = "Early game - forehand turn"
  lazy val trumpJack: Card = Card(round.trumpSuit, Jack(2))
  lazy val canExchangeTrumpJack: Boolean = currentPlayer.hand.contains(trumpJack)
  override lazy val playableCards: Hand = ongoingMarriage.fold(currentPlayer.hand)(m => List(m.king, m.queen))
}

case class EarlyGameDealerTurn(round: GameRound, forehandCard: Card) extends EarlyGame(round, Dealer) with DealerTurn {
  override val name: String = "Early game - dealer turn"
  override def toString = s"${super.toString}\n\tForehand card\t$forehandCard"
  override val playableCards: Hand = currentPlayer.hand
}

sealed abstract class LateGame(game: GameRound, currentRole: Role) extends PlayerTurn {
  val currentPlayer: Player = game.roles(currentRole)
}

case class LateGameForehandTurn(round: GameRound, ongoingMarriage: Option[Marriage] = None) extends LateGame(round, Forehand) with ForehandTurn {
  override val name: String = "Late game - forehand turn"
  override lazy val playableCards: Hand = ongoingMarriage.fold(currentPlayer.hand)(m => List(m.king, m.queen))
}

case class LateGameDealerTurn(round: GameRound, forehandCard: Card) extends LateGame(round, Dealer) with DealerTurn {
  override val name: String = "Late game - dealer turn"
  override lazy val playableCards: Hand =
    currentPlayer.hand.filter(_.suit == forehandCard.suit) match {
      case Nil =>
        currentPlayer.hand.filter(_.suit == round.trumpSuit) match {
          case Nil => currentPlayer.hand
          case trumpSuitCards => trumpSuitCards
        }
      case sameSuit =>
        sameSuit.filter(_.value > forehandCard.value) match {
          case Nil => sameSuit
          case higherCards => higherCards
        }
    }
  override def toString = s"${super.toString}\n\tForehand card\t$forehandCard"
}

case class Finish(round: GameRound, outcome: RoundOutcome) extends GameState {
  override val name: String = "Finish"
}

case class Exit(round: GameRound) extends GameState {
  override val name: String = "Exit"
}
