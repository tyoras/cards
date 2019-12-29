package io.tyoras.cards.game.schnapsen

import java.util.UUID

import io.tyoras.cards.Card

sealed trait Input {
  def name: String
  def playerId: UUID
  override def toString = s"$name"
}

case class Start(playerId: UUID) extends Input {
  val name: String = "Start game"
}

case class PlayCard(playerId: UUID, card: Card) extends Input {
  val name: String = s"Play card $card"
}

case class Restart(playerId: UUID) extends Input {
  val name: String = "Restart game"
}

case class End(playerId: UUID) extends Input {
  val name: String = "Quit game"
}
