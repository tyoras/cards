package io.tyoras.cards.tests.domain.game.schnapsen

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.game.schnapsen.model.DeckError
import io.tyoras.cards.domain.game.schnapsen.{baseDeck, drawFirstCardF}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SchnapsenSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks:

  "baseDeck" should "be the good one for Schnapsen" in {
    val expectedSchnapsenDeck = Set(
      Card(Heart, Ace(11)),
      Card(Heart, Ten()),
      Card(Heart, King(4)),
      Card(Heart, Queen(3)),
      Card(Heart, Jack(2)),
      Card(Diamond, Ace(11)),
      Card(Diamond, Ten()),
      Card(Diamond, King(4)),
      Card(Diamond, Queen(3)),
      Card(Diamond, Jack(2)),
      Card(Spade, Ace(11)),
      Card(Spade, Ten()),
      Card(Spade, King(4)),
      Card(Spade, Queen(3)),
      Card(Spade, Jack(2)),
      Card(Club, Ace(11)),
      Card(Club, Ten()),
      Card(Club, King(4)),
      Card(Club, Queen(3)),
      Card(Club, Jack(2))
    )
    baseDeck should contain theSameElementsAs expectedSchnapsenDeck
  }

  "drawFirstCardF" should "return the first card and the remaining deck" in {
    forAll(deckGen -> "deck") { deck =>
      whenever(deck.nonEmpty) {
        val expectedCard          = deck.head
        val program               = drawFirstCardF[IO](deck)
        val (card, remainingDeck) = program.unsafeRunSync()
        card should be(expectedCard)
        remainingDeck should be(deck.tail)
      }

    }
  }

  it should "return a DeckError when the deck is empty" in {
    val deck    = Nil
    val program = drawFirstCardF[IO](deck)
    a[DeckError] should be thrownBy program.unsafeRunSync()
  }
