package io.tyoras.cards.cli.tui

import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.card.Suit.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CardRenderingSpec extends AnyFlatSpec with Matchers:

  private val ansi                                    = "\u001b\\[[0-9;]*m".r
  private def plain(e: layoutz.Element): String       = ansi.replaceAllIn(e.render, "")
  private def lines(e: layoutz.Element): List[String] = plain(e).split("\n").toList

  "renderCard" should "render every card of the deck as a big card by default" in {
    international52Deck.foreach { card =>
      val rendered = plain(renderCard(card))
      withClue(s"card $card: ") {
        rendered should include(card.rank.toString)
        rendered should include(card.suit.symbol)
        rendered should include("╭")
      }
    }
  }

  it should "render every big card with the same height" in {
    international52Deck.map(c => lines(renderCard(c)).size).distinct should have size 1
  }

  it should "display the number of suits matching the rank of the numbered cards" in {
    val expectedSuits = Map("10" -> 10, "9" -> 9, "8" -> 8, "7" -> 7, "6" -> 6, "5" -> 5, "4" -> 4, "3" -> 3, "2" -> 2, "A" -> 1, "K" -> 1, "Q" -> 1, "J" -> 1)
    allSuits.foreach { suit =>
      defaultRanks.foreach { rank =>
        val card = Card(suit, rank)
        withClue(s"card $card: ") {
          plain(renderCard(card)).count(_ == suit.symbol.head) shouldBe expectedSuits(rank.toString)
        }
      }
    }
  }

  it should "include the title when provided" in {
    plain(renderCard(Card(Heart, Rank.Ace()), title = Some("Alice"))) should include("Alice")
    plain(renderCard(Card(Heart, Rank.Ace()), Size.Small, Some("Bob"))) should include("Bob")
  }

  it should "render a small card with the card value" in {
    val card     = Card(Spade, Rank.Ten())
    val rendered = plain(renderCard(card, Size.Small))
    rendered should include(card.toString)
    lines(renderCard(card, Size.Small)).size should be < lines(renderCard(card)).size
  }

  "renderCardBack" should "render a big card back showing all the suits" in {
    val rendered = plain(renderCardBack())
    allSuits.foreach(s => rendered should include(s.symbol))
    (rendered should not).include("?")
  }

  it should "render a small card back with a question mark" in {
    plain(renderCardBack(Size.Small, Some("hidden"))) should (include("?").and(include("hidden")))
  }

  it should "render a big card back with a title" in {
    plain(renderCardBack(title = Some("prize"))) should include("prize")
  }

  "card dimensions" should "be consistent" in {
    cardWidth shouldBe 12
    cardHeight shouldBe 11
    Size.values.toList shouldBe List(Size.Big, Size.Small)
  }
