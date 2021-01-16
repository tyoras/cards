package io.tyoras.cards.game.schnapsen

import io.tyoras.cards.{Card, Suit}

sealed trait Input {
  def name: String

  def playerId: PlayerId

  override def toString = s"$name"
}

sealed trait MetaInput extends Input

case class Start(playerId: PlayerId) extends MetaInput {
  val name: String = "Start game"
}
case class Restart(playerId: PlayerId) extends MetaInput {
  val name: String = "Restart game"
}

case class End(playerId: PlayerId) extends MetaInput {
  val name: String = "Quit game"
}

sealed trait GameInput extends Input

case class PlayCard(playerId: PlayerId, card: Card) extends GameInput {
  val name: String = s"Play card $card"
}

case class ExchangeTrumpJack(playerId: PlayerId) extends GameInput {
  val name: String = "Exchange trump jack"
}

case class CloseTalon(playerId: PlayerId) extends GameInput {
  val name: String = "Close the talon"
}

case class Meld(playerId: PlayerId, suit: Suit) extends GameInput {
  val name: String = s"Meld $suit King and $suit Queen"
}

case class ClaimVictory(playerId: PlayerId) extends GameInput {
  override def name: String = "Claim victory"
}
