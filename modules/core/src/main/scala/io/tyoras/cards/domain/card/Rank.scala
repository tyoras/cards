package io.tyoras.cards.domain.card

enum Rank(symbol: String) extends Ordered[Rank]:
  val value: Card.Value
  override val toString: String         = symbol
  override def compare(that: Rank): Int = value.value.compare(that.value.value)

  case Ace(value: Card.Value = Card.Value(14))   extends Rank("A")
  case King(value: Card.Value = Card.Value(13))  extends Rank("K")
  case Queen(value: Card.Value = Card.Value(12)) extends Rank("Q")
  case Jack(value: Card.Value = Card.Value(11))  extends Rank("J")
  case Ten(value: Card.Value = Card.Value(10))   extends Rank("10")
  case Nine(value: Card.Value = Card.Value(9))   extends Rank("9")
  case Eight(value: Card.Value = Card.Value(8))  extends Rank("8")
  case Seven(value: Card.Value = Card.Value(7))  extends Rank("7")
  case Six(value: Card.Value = Card.Value(6))    extends Rank("6")
  case Five(value: Card.Value = Card.Value(5))   extends Rank("5")
  case Four(value: Card.Value = Card.Value(4))   extends Rank("4")
  case Three(value: Card.Value = Card.Value(3))  extends Rank("3")
  case Two(value: Card.Value = Card.Value(2))    extends Rank("2")
