package io.tyoras.cards.tests.domain.card

import io.tyoras.cards.domain.card._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RankSpec extends AnyFlatSpec with Matchers:

  "Ranks default values" should "respect the usual order" in {
    val ranksInOrder = List(Two(), Three(), Four(), Five(), Six(), Seven(), Eight(), Nine(), Ten(), Jack(), Queen(), King(), Ace())
    ranksInOrder.sorted should be(ranksInOrder)
  }

  "Two ranks with the same value" should "be considered equivalent by comparison" in {
    Ten().compareTo(Nine(10)) should be(0)
  }
