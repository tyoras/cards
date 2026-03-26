package io.tyoras.cards.domain.card

import io.tyoras.cards.domain.card.Color.*

import scala.Console.{BLACK, RED, RESET}
import scala.util.Try

enum Color(consoleColor: String):
  case Black extends Color(BLACK)
  case Red   extends Color(RED)

  def colorize(s: String): String = s"$RESET$consoleColor$s$RESET"

enum Suit(val color: Color, symbol: String):
  case Heart   extends Suit(Red, "♥")
  case Diamond extends Suit(Red, "♦")
  case Club    extends Suit(Black, "♣")
  case Spade   extends Suit(Black, "♠")

  override lazy val toString: String = color.colorize(symbol)

object Suit:
  def fromName(name: String): Either[String, Suit] =
    Try(Suit.valueOf(name.trim.capitalize)).toEither.left.map(e => s"Invalid suit $name : $e")
