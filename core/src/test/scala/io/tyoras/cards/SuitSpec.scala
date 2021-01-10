package io.tyoras.cards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.Console.{BLACK, RED, RESET}

class SuitSpec extends AnyFlatSpec with Matchers {

  "Heart" should "have symbol" in {
    Heart.toString should be(s"$RESET$RED♥$RESET")
  }

  it should "be red" in {
    Heart.color should be(Red)
  }

  "Diamond" should "looks like this" in {
    Diamond.toString should be(s"$RESET$RED♦$RESET")
  }

  it should "be red" in {
    Diamond.color should be(Red)
  }

  "Club" should "looks like this" in {
    Club.toString should be(s"$RESET$BLACK♣$RESET")
  }

  it should "be black" in {
    Club.color should be(Black)
  }

  "Spade" should "looks like this" in {
    Spade.toString should be(s"$RESET$BLACK♠$RESET")
  }

  it should "be black" in {
    Spade.color should be(Black)
  }
}
