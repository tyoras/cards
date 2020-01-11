package io.tyoras.cards.game.schnapsen

import io.tyoras.cards.Card

sealed trait Input {
  def name: String

  def playerId: PlayerId

  override def toString = s"$name"
}

case class Start(playerId: PlayerId) extends Input {
  val name: String = "Start game"
}

case class PlayCard(playerId: PlayerId, card: Card) extends Input {
  val name: String = s"Play card $card"
}

case class ExchangeTrumpJack(playerId: PlayerId) extends Input {
  val name: String = "Exchange trump jack"
}

case class CloseTalon(playerId: PlayerId) extends Input {
  val name: String = "Close the talon"
}

case class Restart(playerId: PlayerId) extends Input {
  val name: String = "Restart game"
}

case class End(playerId: PlayerId) extends Input {
  val name: String = "Quit game"
}
