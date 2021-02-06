package io.tyoras.cards.tests.domain.card

import io.tyoras.cards.domain.card.{Card, Heart, King, Spade, Ten}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.Console.{RED, RESET}

class CardSpec extends AnyFlatSpec with Matchers {

  val heartKing: Card = Card(Heart, King())
  val spadeKing: Card = Card(Spade, King())
  val heartTen: Card = Card(Heart, Ten())

  "Cards comparison" should "be based on rank only" in {
    heartKing > heartTen should be(true)
  }

  "Two cards with the same rank value" should "be considered equivalent by comparison" in {
    heartKing.compareTo(spadeKing) should be(0)
  }

  "toString" should "work" in {
    val expected = s"K$RESET$RED♥$RESET"
    heartKing.toString should be(expected)
  }

}
