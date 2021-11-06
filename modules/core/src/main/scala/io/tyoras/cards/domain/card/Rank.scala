package io.tyoras.cards.domain.card

sealed trait Rank extends Ordered[Rank]:
  def value: Int

  override def compare(that: Rank): Int = value.compare(that.value)

case class Ace(value: Int = 14) extends Rank:
  override val toString = "A"

case class King(value: Int = 13) extends Rank:
  override val toString = "K"

case class Queen(value: Int = 12) extends Rank:
  override val toString = "Q"

case class Jack(value: Int = 11) extends Rank:
  override val toString = "J"

case class Ten(value: Int = 10) extends Rank:
  override val toString = "10"

case class Nine(value: Int = 9) extends Rank:
  override val toString = "9"

case class Eight(value: Int = 8) extends Rank:
  override val toString = "8"

case class Seven(value: Int = 7) extends Rank:
  override val toString = "7"

case class Six(value: Int = 6) extends Rank:
  override val toString = "6"

case class Five(value: Int = 5) extends Rank:
  override val toString = "5"

case class Four(value: Int = 4) extends Rank:
  override val toString = "4"

case class Three(value: Int = 3) extends Rank:
  override val toString = "3"

case class Two(value: Int = 2) extends Rank:
  override val toString = "2"
