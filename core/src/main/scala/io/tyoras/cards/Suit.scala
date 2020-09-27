package io.tyoras.cards

import scala.Console.{BLACK, RED, RESET}

sealed trait Color {
  def colorize(s: String): String
}
case object Black extends Color {
  override def colorize(s: String): String = s"$RESET$BLACK$s$RESET"
}
case object Red extends Color {
  override def colorize(s: String): String = s"$RESET$RED$s$RESET"
}

sealed trait Suit {
  def color: Color
  def symbol: String

  override lazy val toString: String = color.colorize(symbol)
}
case object Heart extends Suit {
  override val color: Color = Red
  override val symbol: String = "♥"
}
case object Diamond extends Suit {
  override val color: Color = Red
  override val symbol: String = "♦"
}
case object Club extends Suit {
  override val color: Color = Black
  override val symbol: String = "♣"
}
case object Spade extends Suit {
  override val color: Color = Black
  override val symbol: String = "♠"
}
