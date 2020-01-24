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

case class Init(game: Game) extends GameState {
  val name: String = "Initialisation"
}

sealed abstract class EarlyGame(game: Game, currentRole: Role) extends PlayerTurn {
  val currentPlayer: Player = game.roles(currentRole)
  def playableCards: Hand
}

case class EarlyGameForehandTurn(game: Game, ongoingMarriage: Option[Marriage] = None) extends EarlyGame(game, Forehand) {
  override val name: String = "Early game - forehand turn"
  lazy val trumpJack: Card = Card(game.trumpSuit, Jack(2))
  lazy val canExchangeTrumpJack: Boolean = currentPlayer.hand.contains(trumpJack)
  lazy val possibleMarriages: List[Marriage] = findPossibleMarriages(currentPlayer.hand, game.trumpSuit)
  override val playableCards: Hand = ongoingMarriage.fold(currentPlayer.hand)(m => List(m.king, m.queen))
}

case class EarlyGameDealerTurn(game: Game, forehandCard: Card) extends EarlyGame(game, Dealer) {
  override val name: String = "Early game - dealer turn"
  override def toString = s"${super.toString}\n\tForehand card\t$forehandCard"
  override val playableCards: Hand = currentPlayer.hand
}

sealed abstract class LateGame(game: Game, currentRole: Role) extends PlayerTurn {
  val currentPlayer: Player = game.roles(currentRole)
}

case class LateGameForehandTurn(game: Game) extends LateGame(game, Forehand) {
  override val name: String = "Late game - forehand turn"
  override val playableCards: Hand = currentPlayer.hand
  lazy val possibleMarriages: List[Marriage] = findPossibleMarriages(currentPlayer.hand, game.trumpSuit)
}

case class LateGameDealerTurn(game: Game, forehandCard: Card) extends LateGame(game, Dealer) {
  override val name: String = "Late game - dealer turn"
  override val playableCards: Hand = currentPlayer.hand //TODO compute playable cards
  override def toString = s"${super.toString}\n\tForehand card\t$forehandCard"
}

case class Exit(game: Game) extends GameState {
  val name: String = "Exit"
}
