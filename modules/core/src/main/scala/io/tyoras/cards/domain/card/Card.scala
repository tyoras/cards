package io.tyoras.cards.domain.card

import cats.{Order, Show}
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.Rank.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.string.Blank
import io.github.iltotore.iron.constraint.numeric.Positive0

case class Card(id: Card.ID, suit: Suit, rank: Rank, customValue: Option[Card.Value] = None) extends Ordered[Card]:
  override def toString: String = Card.emoji(this, colored = false)

  override def compare(that: Card): Int = value.value.compare(that.value.value)

  val color: Color = suit.color

  val value: Card.Value = customValue.getOrElse(rank.value)

  val emoji: String = Card.emoji(this)

object Card:
  type ID = Card.ID.T
  object ID extends RefinedType[String, Not[Blank]]

  type Value = Card.Value.T
  object Value extends RefinedSubtype[Int, Positive0]

  type Count = Card.Count.T
  object Count extends RefinedSubtype[Int, Positive0]

  given Show[Card]  = Show.fromToString
  given Order[Card] = Order.fromOrdering

  def apply(suit: Suit, rank: Rank): Card =
    val id = Card.ID.applyUnsafe(s"${rank.toString}${suit.symbol}")
    new Card(id, suit, rank, None)

  val cardBackEmoji: String = "🂠"

  def emoji(card: Card, colored: Boolean = true): String =
    val cardEmoji = card match
      case Card(_, Spade, Ace(_), _)     => "🂡"
      case Card(_, Spade, King(_), _)    => "🂮"
      case Card(_, Spade, Queen(_), _)   => "🂭"
      case Card(_, Spade, Jack(_), _)    => "🂫"
      case Card(_, Spade, Ten(_), _)     => "🂪"
      case Card(_, Spade, Nine(_), _)    => "🂩"
      case Card(_, Spade, Eight(_), _)   => "🂨"
      case Card(_, Spade, Seven(_), _)   => "🂧"
      case Card(_, Spade, Six(_), _)     => "🂦"
      case Card(_, Spade, Five(_), _)    => "🂥"
      case Card(_, Spade, Four(_), _)    => "🂤"
      case Card(_, Spade, Three(_), _)   => "🂣"
      case Card(_, Spade, Two(_), _)     => "🂢"
      case Card(_, Club, Ace(_), _)      => "🃑"
      case Card(_, Club, King(_), _)     => "🃞"
      case Card(_, Club, Queen(_), _)    => "🃝"
      case Card(_, Club, Jack(_), _)     => "🃛"
      case Card(_, Club, Ten(_), _)      => "🃚"
      case Card(_, Club, Nine(_), _)     => "🃙"
      case Card(_, Club, Eight(_), _)    => "🃘"
      case Card(_, Club, Seven(_), _)    => "🃗"
      case Card(_, Club, Six(_), _)      => "🃖"
      case Card(_, Club, Five(_), _)     => "🃕"
      case Card(_, Club, Four(_), _)     => "🃔"
      case Card(_, Club, Three(_), _)    => "🃓"
      case Card(_, Club, Two(_), _)      => "🃒"
      case Card(_, Heart, Ace(_), _)     => "🂱"
      case Card(_, Heart, King(_), _)    => "🂾"
      case Card(_, Heart, Queen(_), _)   => "🂽"
      case Card(_, Heart, Jack(_), _)    => "🂻"
      case Card(_, Heart, Ten(_), _)     => "🂺"
      case Card(_, Heart, Nine(_), _)    => "🂹"
      case Card(_, Heart, Eight(_), _)   => "🂸"
      case Card(_, Heart, Seven(_), _)   => "🂷"
      case Card(_, Heart, Six(_), _)     => "🂶"
      case Card(_, Heart, Five(_), _)    => "🂵"
      case Card(_, Heart, Four(_), _)    => "🂴"
      case Card(_, Heart, Three(_), _)   => "🂳"
      case Card(_, Heart, Two(_), _)     => "🂲"
      case Card(_, Diamond, Ace(_), _)   => "🃁"
      case Card(_, Diamond, King(_), _)  => "🃎"
      case Card(_, Diamond, Queen(_), _) => "🃍"
      case Card(_, Diamond, Jack(_), _)  => "🃋"
      case Card(_, Diamond, Ten(_), _)   => "🃊"
      case Card(_, Diamond, Nine(_), _)  => "🃉"
      case Card(_, Diamond, Eight(_), _) => "🃈"
      case Card(_, Diamond, Seven(_), _) => "🃇"
      case Card(_, Diamond, Six(_), _)   => "🃆"
      case Card(_, Diamond, Five(_), _)  => "🃅"
      case Card(_, Diamond, Four(_), _)  => "🃄"
      case Card(_, Diamond, Three(_), _) => "🃃"
      case Card(_, Diamond, Two(_), _)   => "🃂"
    if colored then card.suit.color.colorize(cardEmoji) else cardEmoji
