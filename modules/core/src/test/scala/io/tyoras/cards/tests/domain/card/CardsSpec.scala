package io.tyoras.cards.tests.domain.card

import io.tyoras.cards.domain.card._
import io.tyoras.cards.tests._
import org.scalacheck.Gen
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CardsSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "blackSuits" should "contain only spade and club" in {
    blackSuits should contain.only(Spade, Club)
  }

  "redSuits" should "contain only heart and diamond" in {
    redSuits should contain.only(Heart, Diamond)
  }

  "allSuits" should "contain both black and red suits" in {
    val expectedSuits = blackSuits ++ redSuits
    allSuits should contain theSameElementsAs expectedSuits
  }

  val expectedInternational52DeckSize: Long = 52

  "international52Deck" should "contain 52 cards" in {
    forAll(international52DeckGen -> "deck") { international52Deck =>
      international52Deck should have size expectedInternational52DeckSize
    }
  }

  it should "contain only unique cards" in {
    forAll(international52DeckGen -> "deck") { international52Deck =>
      international52Deck.toSet should have size expectedInternational52DeckSize
    }
  }

  "createDeck" should "create a deck containing the right number of card" in {
    forAll(suitsGen -> "suits", defaultRanksGen -> "ranks") { (suits, ranks) =>
      val expectedDeckLength = suits.size * ranks.size
      createDeck(suits, ranks) should have size expectedDeckLength.toLong
    }
  }

  it should "create a deck containing only unique cards" in {
    forAll(suitsGen -> "suits", defaultRanksGen -> "ranks") { (suits, ranks) =>
      val expectedDeckLength = suits.size * ranks.size
      createDeck(suits, ranks).toSet should have size expectedDeckLength.toLong
    }
  }

  "shuffle" should "preserve the deck size" in {
    val baseDeck = international52Deck
    val expectedDeckSize = baseDeck.size.toLong
    shuffle(baseDeck) should have size expectedDeckSize
  }

  it should "keep the same cards" in {
    forAll(randomDeckGen -> "deck") { deck =>
      shuffle(deck) should contain theSameElementsAs deck
    }
  }

  //actually as it is random the same order can be produced multiple time thus failing this test
  it should "produce different deck order most of the time" ignore {
    forAll(randomDeckGen -> "deck") { deck =>
      whenever(deck.size > 1) {
        val firstShuffle = shuffle(deck)
        val secondShuffle = shuffle(deck)
        firstShuffle should (contain theSameElementsAs secondShuffle and not contain theSameElementsInOrderAs(secondShuffle))
      }
    }
  }

  it should "return an empty deck on a empty deck" in {
    val baseDeck = Nil
    shuffle(baseDeck) shouldBe empty
  }

  "pickCard by index" should "return None with an empty hand" in {
    val hand = Nil
    pickCard(0, hand) should be((None, hand))
  }

  it should "return the card and the remaining hand when the hand contain cards" in {
    forAll(randomDeckGen -> "hand", Gen.choose[Int](0, 52) -> "index") { (hand, index) =>
      whenever(index < hand.size) {
        val expectedCard = hand(index)
        val expectedRemainingHand = hand diff List(expectedCard)
        val (pickedCard, remainingHand) = pickCard(index, hand)
        pickedCard.value shouldBe expectedCard
        remainingHand should contain theSameElementsInOrderAs expectedRemainingHand
      }
    }
  }

  it should "return None when the index is lower than 0" in {
    forAll(randomDeckGen -> "hand", Gen.negNum[Int] -> "index") { (hand, index) =>
      pickCard(index, hand) shouldBe (None -> hand)
    }
  }

  it should "return None when the index is equal to the hand size" in {
    forAll(randomDeckGen -> "hand") { hand =>
      pickCard(hand.size, hand) shouldBe (None -> hand)
    }
  }

  it should "return None when the index is greater than the hand size" in {
    forAll(randomDeckGen -> "hand", Gen.posNum[Int] -> "index") { (hand, index) =>
      whenever(index > hand.size) {
        pickCard(index, hand) shouldBe (None -> hand)
      }
    }
  }

  "pickCard by card" should "return None with an empty hand" in {
    forAll(cardGen -> "card") { card =>
      val hand = Nil
      pickCard(card, hand) shouldBe (None -> hand)
    }
  }

  it should "return the card and the remaining hand when the hand contain cards" in {
    forAll(randomDeckGen -> "hand") { hand =>
      val distinctHand = hand.toSet.toList
      whenever(distinctHand.nonEmpty) {
        val expectedCard :: handWithoutCardToPick = distinctHand
        val (pickedCard, remainingHand) = pickCard(expectedCard, distinctHand)
        pickedCard.value shouldBe expectedCard
        remainingHand should contain theSameElementsInOrderAs handWithoutCardToPick
      }
    }
  }

  it should "return None when the card is not present in the hand" in {
    forAll(randomDeckGen -> "hand") { hand =>
      val distinctHand = hand.toSet.toList
      whenever(distinctHand.nonEmpty) {
        val card :: handWithoutCardToPick = distinctHand
        pickCard(card, handWithoutCardToPick) shouldBe (None -> handWithoutCardToPick)
      }
    }
  }

  it should "return only the first matching card when the card has several occurrences in the hand" in {
    forAll(randomDeckGen -> "hand") { hand =>
      whenever(hand.nonEmpty) {
        val card = hand.head
        val handWithSeveralCardOccurrences = card :: hand
        val (pickedCard, remainingHand) = pickCard(card, handWithSeveralCardOccurrences)
        pickedCard.value shouldBe card
        remainingHand should contain theSameElementsInOrderAs hand
      }
    }
  }

  "drawNCard" should "return the n first card from a deck and the remaining deck when the deck has more cards than asked" in {
    forAll(randomDeckGen -> "deck", Gen.choose(1, 3) -> "n") { (deck, n) =>
      whenever(deck.size > 3) {
        val (drawnCards, remainingDeck) = drawNCard(n, deck)
        drawnCards should contain theSameElementsInOrderAs deck.take(n)
        remainingDeck should contain theSameElementsInOrderAs deck.drop(n)
      }
    }
  }

  it should "return all the cards from a deck when the number of card to draw is greater than the deck size" in {
    forAll(randomDeckGen -> "deck", Gen.posNum[Int] -> "n") { (deck, n) =>
      whenever(deck.size < n) {
        val (drawnCards, remainingDeck) = drawNCard(n, deck)
        drawnCards should contain theSameElementsInOrderAs deck
        remainingDeck shouldBe empty
      }
    }
  }

  it should "return an empty list when the deck is empty" in {
    forAll(Gen.posNum[Int] -> "n") { n =>
      val deck = Nil
      val (drawnCards, remainingDeck) = drawNCard(n, deck)
      drawnCards shouldBe empty
      remainingDeck shouldBe deck
    }
  }

  it should "return an empty list when the number of card to draw is 0" in {
    forAll(randomDeckGen -> "deck") { deck =>
      whenever(deck.nonEmpty) {
        val (drawnCards, remainingDeck) = drawNCard(0, deck)
        drawnCards shouldBe empty
        remainingDeck shouldBe deck
      }
    }
  }

  it should "return an empty list when the number of card to draw is lower than 0" in {
    forAll(randomDeckGen -> "deck", Gen.negNum[Int] -> "n") { (deck, n) =>
      whenever(deck.nonEmpty) {
        val (drawnCards, remainingDeck) = drawNCard(n, deck)
        drawnCards shouldBe empty
        remainingDeck should be(deck)
      }
    }
  }

  "drawFirstCard" should "return the first card when the deck has more than one card" in {
    forAll(randomDeckGen -> "deck") { deck =>
      whenever(deck.nonEmpty) {
        val (drawnCard, remainingDeck) = drawFirstCard(deck)
        drawnCard.value shouldBe deck.head
        remainingDeck should contain theSameElementsInOrderAs deck.tail
      }
    }
  }

  it should "return the first card when the deck has only one card" in {
    forAll(cardGen -> "card") { card =>
      val deck = List(card)
      val (drawnCard, remainingDeck) = drawFirstCard(deck)
      drawnCard.value should be(card)
      remainingDeck shouldBe empty
    }
  }

  it should "return None when the deck is empty" in {
    val deck = Nil
    val (drawnCard, remainingDeck) = drawFirstCard(deck)
    drawnCard should be(None)
    remainingDeck should be(deck)
  }
}
