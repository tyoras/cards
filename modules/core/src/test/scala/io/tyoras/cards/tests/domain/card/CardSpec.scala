package io.tyoras.cards.tests.domain.card

import cats.syntax.show.*
import io.tyoras.cards.domain.card.Rank.{King, Ten}
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.{Card, Suit}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.Console.{RED, RESET}

class CardSpec extends AnyFlatSpec with Matchers:

  val heartKing: Card = Card(Heart, King())
  val spadeKing: Card = Card(Spade, King())
  val heartTen: Card  = Card(Heart, Ten())

  "Cards comparison" should "be based on rank only" in {
    heartKing > heartTen should be(true)
  }

  "Two cards with the same rank value" should "be considered equivalent by comparison" in {
    heartKing.compareTo(spadeKing) should be(0)
  }

  "toString" should "work and not be colored" in {
    val expected = "🂾"
    heartKing.toString should be(expected)
  }

  "emoji" should "work and be colored" in {
    val expected = s"$RESET$RED🂾$RESET"
    heartKing.emoji should be(expected)
  }

  "value" should "be the rank value by default" in {
    Card(Card.ID("custom"), Heart, King()).value should be(King().value)
  }

  it should "be the custom value when there is one" in {
    Card(Card.ID("custom"), Heart, King(), Some(Card.Value(1))).value should be(Card.Value(1))
  }

  "Show" should "display the card like toString" in {
    heartKing.show should be(heartKing.toString)
  }

  "Suit.fromName" should "find a suit from its name, ignoring the case of the first letter and the spaces" in {
    Suit.fromName("Heart") should be(Right(Heart))
    Suit.fromName(" spade ") should be(Right(Spade))
  }

  it should "return an error for an unknown suit" in {
    Suit.fromName("star").left.map(_ should startWith("Invalid suit star")).isLeft should be(true)
  }
