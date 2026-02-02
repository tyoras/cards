package io.tyoras.cards.domain.card

enum Rank(symbol: String) extends Ordered[Rank]:
  val value: Int
  override val toString: String         = symbol
  override def compare(that: Rank): Int = value.compare(that.value)

  case Ace(override val value: Int = 14)  extends Rank("A")
  case King(override val value: Int = 13) extends Rank("K")
  case Queen(value: Int = 12)             extends Rank("Q")
  case Jack(value: Int = 11)              extends Rank("J")
  case Ten(value: Int = 10)               extends Rank("10")
  case Nine(value: Int = 9)               extends Rank("9")
  case Eight(value: Int = 8)              extends Rank("8")
  case Seven(value: Int = 7)              extends Rank("7")
  case Six(value: Int = 6)                extends Rank("6")
  case Five(value: Int = 5)               extends Rank("5")
  case Four(value: Int = 4)               extends Rank("4")
  case Three(value: Int = 3)              extends Rank("3")
  case Two(value: Int = 2)                extends Rank("2")
