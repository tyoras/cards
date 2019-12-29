package io.tyoras.cards

sealed trait Rank extends Ordered[Rank] {
  def value: Int

  override def compare(that: Rank): Int = value.compare(that.value)
}

sealed case class Ace(value: Int = 14) extends Rank {
  override val toString = "A"
}

sealed case class King(value: Int = 13) extends Rank {
  override val toString = "K"
}

sealed case class Queen(value: Int = 12) extends Rank {
  override val toString = "Q"
}

sealed case class Jack(value: Int = 11) extends Rank {
  override val toString = "J"
}

sealed case class Ten(value: Int = 10) extends Rank {
  override val toString = "10"
}

sealed case class Nine(value: Int = 9) extends Rank {
  override val toString = "9"
}

sealed case class Eight(value: Int = 8) extends Rank {
  override val toString = "8"
}

sealed case class Seven(value: Int = 7) extends Rank {
  override val toString = "7"
}

sealed case class Six(value: Int = 6) extends Rank {
  override val toString = "6"
}

sealed case class Five(value: Int = 5) extends Rank {
  override val toString = "5"
}

sealed case class Four(value: Int = 4) extends Rank {
  override val toString = "4"
}

sealed case class Three(value: Int = 3) extends Rank {
  override val toString = "3"
}

sealed case class Two(value: Int = 2) extends Rank {
  override val toString = "2"
}
