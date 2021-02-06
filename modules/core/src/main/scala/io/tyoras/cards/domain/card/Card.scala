package io.tyoras.cards.domain.card

case class Card(suit: Suit, rank: Rank) extends Ordered[Card] {
  override def toString: String = s"$rank$suit"

  override def compare(that: Card): Int = rank.compare(that.rank)

  val color: Color = suit.color

  val value: Int = rank.value
}
