package io.tyoras.cards.game.schnapsen

import io.tyoras.cards.game.schnapsen.model._
import io.tyoras.cards.{Card, Hand, Jack}

sealed trait GameState {
  def name: String
  def game: Game
  def playableCards: Hand = Nil
  override def toString = s"$name$game\n\tPlayable cards \t${if (playableCards.isEmpty) "None" else playableCards.mkString(" ")}"
}

sealed trait PlayerTurn extends GameState {
  def currentPlayer: Player
}

sealed trait ForehandTurn extends PlayerTurn {
  lazy val possibleMarriages: List[Marriage] = findPossibleMarriages(currentPlayer.hand, game.trumpSuit)
}

sealed trait DealerTurn extends PlayerTurn {
  def forehandCard: Card
}

case class Init(game: Game) extends GameState {
  val name: String = "Initialisation"
}

sealed abstract class EarlyGame(game: Game, currentRole: Role) extends PlayerTurn {
  val currentPlayer: Player = game.roles(currentRole)
  def playableCards: Hand
}

case class EarlyGameForehandTurn(game: Game, ongoingMarriage: Option[Marriage] = None) extends EarlyGame(game, Forehand) with ForehandTurn {
  override val name: String = "Early game - forehand turn"
  lazy val trumpJack: Card = Card(game.trumpSuit, Jack(2))
  lazy val canExchangeTrumpJack: Boolean = currentPlayer.hand.contains(trumpJack)
  override lazy val playableCards: Hand = ongoingMarriage.fold(currentPlayer.hand)(m => List(m.king, m.queen))
}

case class EarlyGameDealerTurn(game: Game, forehandCard: Card) extends EarlyGame(game, Dealer) with DealerTurn {
  override val name: String = "Early game - dealer turn"
  override def toString = s"${super.toString}\n\tForehand card\t$forehandCard"
  override val playableCards: Hand = currentPlayer.hand
}

sealed abstract class LateGame(game: Game, currentRole: Role) extends PlayerTurn {
  val currentPlayer: Player = game.roles(currentRole)
}

case class LateGameForehandTurn(game: Game, ongoingMarriage: Option[Marriage] = None) extends LateGame(game, Forehand) with ForehandTurn {
  override val name: String = "Late game - forehand turn"
  override lazy val playableCards: Hand = ongoingMarriage.fold(currentPlayer.hand)(m => List(m.king, m.queen))
}

case class LateGameDealerTurn(game: Game, forehandCard: Card) extends LateGame(game, Dealer) with DealerTurn {
  override val name: String = "Late game - dealer turn"
  override lazy val playableCards: Hand =
    currentPlayer.hand.filter(_.suit == forehandCard.suit) match {
      case Nil =>
        currentPlayer.hand.filter(_.suit == game.trumpSuit) match {
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

case class Finish(game: Game) extends GameState {
  override val name: String = "Finish"
  //TODO complete win conditions
  lazy val winner: Player = game.lastHandWonBy.map(game.playersById).getOrElse {
    if (game.forehand.score > game.dealer.score) game.forehand else game.dealer
  }
}

case class Exit(game: Game) extends GameState {
  override val name: String = "Exit"
}
