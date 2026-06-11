package io.tyoras.cards.tests.domain.card

import io.tyoras.cards.domain.card.Rank.{King, Ten}
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.Card
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

  "Cards json serialization" should "work" in {
    import io.circe.parser.decode
    import io.circe.syntax.*
    import io.tyoras.cards.domain.card.codecs.given

    val card = Card(Heart, King())
    val json = card.asJson.spaces2

    decode[Card](json) should be(Right(card))
  }
