package io.tyoras.cards.domain.card

import cats.{Order, Show}
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.Rank.*

case class Card(suit: Suit, rank: Rank) extends Ordered[Card]:
  override def toString: String = emoji

  override def compare(that: Card): Int = rank.compare(that.rank)

  val color: Color = suit.color

  val value: Int = rank.value

  val emoji: String = Card.emoji(this)

object Card:
  given Show[Card]  = Show.fromToString
  given Order[Card] = Order.fromOrdering

  val cardBackEmoji: String = "🂠"

  def emoji(card: Card, colored: Boolean = true): String =
    val cardEmoji = card match
      case Card(Spade, Ace(_))     => "🂡"
      case Card(Spade, King(_))    => "🂮"
      case Card(Spade, Queen(_))   => "🂭"
      case Card(Spade, Jack(_))    => "🂫"
      case Card(Spade, Ten(_))     => "🂪"
      case Card(Spade, Nine(_))    => "🂩"
      case Card(Spade, Eight(_))   => "🂨"
      case Card(Spade, Seven(_))   => "🂧"
      case Card(Spade, Six(_))     => "🂦"
      case Card(Spade, Five(_))    => "🂥"
      case Card(Spade, Four(_))    => "🂤"
      case Card(Spade, Three(_))   => "🂣"
      case Card(Spade, Two(_))     => "🂢"
      case Card(Club, Ace(_))      => "🃑"
      case Card(Club, King(_))     => "🃞"
      case Card(Club, Queen(_))    => "🃝"
      case Card(Club, Jack(_))     => "🃛"
      case Card(Club, Ten(_))      => "🃚"
      case Card(Club, Nine(_))     => "🃙"
      case Card(Club, Eight(_))    => "🃘"
      case Card(Club, Seven(_))    => "🃗"
      case Card(Club, Six(_))      => "🃖"
      case Card(Club, Five(_))     => "🃕"
      case Card(Club, Four(_))     => "🃔"
      case Card(Club, Three(_))    => "🃓"
      case Card(Club, Two(_))      => "🃒"
      case Card(Heart, Ace(_))     => "🂱"
      case Card(Heart, King(_))    => "🂾"
      case Card(Heart, Queen(_))   => "🂽"
      case Card(Heart, Jack(_))    => "🂻"
      case Card(Heart, Ten(_))     => "🂺"
      case Card(Heart, Nine(_))    => "🂹"
      case Card(Heart, Eight(_))   => "🂸"
      case Card(Heart, Seven(_))   => "🂷"
      case Card(Heart, Six(_))     => "🂶"
      case Card(Heart, Five(_))    => "🂵"
      case Card(Heart, Four(_))    => "🂴"
      case Card(Heart, Three(_))   => "🂳"
      case Card(Heart, Two(_))     => "🂲"
      case Card(Diamond, Ace(_))   => "🃁"
      case Card(Diamond, King(_))  => "🃎"
      case Card(Diamond, Queen(_)) => "🃍"
      case Card(Diamond, Jack(_))  => "🃋"
      case Card(Diamond, Ten(_))   => "🃊"
      case Card(Diamond, Nine(_))  => "🃉"
      case Card(Diamond, Eight(_)) => "🃈"
      case Card(Diamond, Seven(_)) => "🃇"
      case Card(Diamond, Six(_))   => "🃆"
      case Card(Diamond, Five(_))  => "🃅"
      case Card(Diamond, Four(_))  => "🃄"
      case Card(Diamond, Three(_)) => "🃃"
      case Card(Diamond, Two(_))   => "🃂"
    if colored then card.suit.color.colorize(cardEmoji) else cardEmoji
