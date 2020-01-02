package io.tyoras.cards

import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CardsSpec extends AnyFlatSpec with Matchers {

  "blackSuits" should "contain only spade and club" in {
    blackSuits should contain only (Spade, Club)
  }

  "redSuits" should "contain only heart and diamond" in {
    redSuits should contain only (Heart, Diamond)
  }

  "allSuits" should "contain both black and red suits" in {
    val expectedSuits = blackSuits ++ redSuits
    allSuits should contain theSameElementsAs expectedSuits
  }

  val expectedInternational52DeckSize: Int = 52

  "international52Deck" should "contain 52 cards" in {
    international52Deck should have size expectedInternational52DeckSize
  }

  it should "contain only  unique cards" in {
    international52Deck.toSet should have size expectedInternational52DeckSize
  }

  "createDeck" should "create a deck containing the right number of card" in {
    val suits = redSuits
    val ranks = defaultRanks
    val expectedDeckLength = suits.size * ranks.size
    createDeck(suits, ranks) should have size expectedDeckLength
  }

  it should "create a deck containing only unique cards" in {
    val suits = redSuits
    val ranks = defaultRanks
    val expectedDeckLength = suits.size * ranks.size
    createDeck(suits, ranks).toSet should have size expectedDeckLength
  }

  "shuffle" should "preserve the deck size" in {
    val baseDeck = international52Deck
    val expectedDeckSize = baseDeck.size
    shuffle(baseDeck) should have size expectedDeckSize
  }

  it should "keep the same cards" in {
    val baseDeck = international52Deck
    shuffle(baseDeck) should contain theSameElementsAs baseDeck
  }

  it should "produce different deck order every time" in {
    val baseDeck = international52Deck
    val firstShuffle = shuffle(baseDeck)
    val secondShuffle = shuffle(baseDeck)
    firstShuffle should (contain theSameElementsAs secondShuffle and not contain theSameElementsInOrderAs(secondShuffle))
  }

  it should "return an empty deck on a empty deck" in {
    val baseDeck = Nil
    shuffle(baseDeck) shouldBe empty
  }

  "pickCard by index" should "return None with an empty hand" in {
    val hand = Nil
    pickCard(0, hand) should be (None, hand)
  }

  it should "return the card and the remaining hand when the hand contain cards" in {
    val expectedCard = Card(Spade, Ace())
    val hand = List(Card(Diamond, Jack()), expectedCard, Card(Heart, Queen()))
    val (pickedCard, remainingHand) = pickCard(1, hand)
    pickedCard.value should be (expectedCard)
    remainingHand should contain inOrderOnly(Card(Diamond, Jack()), Card(Heart, Queen()))
  }

  it should "return None when the index is lower than 0" in {
    val hand = List(Card(Diamond, Jack()), Card(Heart, Queen()))
    pickCard(-1, hand) should be (None, hand)
  }

  it should "return None when the index is equal to the hand size" in {
    val hand = List(Card(Diamond, Jack()), Card(Heart, Queen()))
    pickCard(2, hand) should be (None, hand)
  }

  it should "return None when the index is greater than the hand size" in {
    val hand = List(Card(Diamond, Jack()), Card(Heart, Queen()))
    pickCard(10, hand) should be (None, hand)
  }

  "pickCard by card" should "return None with an empty hand" in {
    val card = Card(Spade, Ace())
    val hand = Nil
    pickCard(card, hand) should be (None, hand)
  }

  it should "return the card and the remaining hand when the hand contain cards" in {
    val card = Card(Spade, Ace())
    val expectedCard = Card(Spade, Ace())
    val hand = List(Card(Diamond, Jack()), card, Card(Heart, Queen()))
    val (pickedCard, remainingHand) = pickCard(card, hand)
    pickedCard.value should be (expectedCard)
    remainingHand should contain inOrderOnly(Card(Diamond, Jack()), Card(Heart, Queen()))
  }

  it should "return None when the card is not present in the hand" in {
    val card = Card(Spade, Ace())
    val hand = List(Card(Diamond, Jack()), Card(Heart, Queen()))
    pickCard(card, hand) should be (None, hand)
  }

  it should "return only the first matching card when the card has several occurrences in the hand" in {
    val card = Card(Spade, Ace())
    val expectedCard = Card(Spade, Ace())
    val hand = List(Card(Diamond, Jack()), Card(Spade, Ace()), Card(Heart, Queen()), Card(Spade, Ace()))
    val (pickedCard, remainingHand) = pickCard(card, hand)
    pickedCard.value should be (expectedCard)
    remainingHand should contain inOrderOnly(Card(Diamond, Jack()), Card(Heart, Queen()), Card(Spade, Ace()))
  }

  "drawNCard" should "return the n first card from a deck and the remaining deck when the deck has more cards than asked" in {
    val deck = List(Card(Diamond, Jack()), Card(Spade, Ace()), Card(Heart, Queen()), Card(Club, Ace()))
    val (drawnCards, remainingDeck) = drawNCard(2, deck)
    drawnCards should contain inOrderOnly(Card(Diamond, Jack()), Card(Spade, Ace()))
    remainingDeck should contain inOrderOnly(Card(Heart, Queen()), Card(Club, Ace()))
  }

  it should "return all the cards from a deck when the number of card to draw is greater than the deck size" in {
    val deck = List(Card(Diamond, Jack()), Card(Spade, Ace()), Card(Heart, Queen()), Card(Club, Ace()))
    val (drawnCards, remainingDeck) = drawNCard(42, deck)
    drawnCards should contain theSameElementsInOrderAs deck
    remainingDeck shouldBe empty
  }

  it should "return an empty list when the deck is empty" in {
    val deck = Nil
    val (drawnCards, remainingDeck) = drawNCard(42, deck)
    drawnCards shouldBe empty
    remainingDeck should be (deck)
  }

  it should "return an empty list when the number of card to draw is 0" in {
    val deck = List(Card(Diamond, Jack()), Card(Spade, Ace()), Card(Heart, Queen()), Card(Club, Ace()))
    val (drawnCards, remainingDeck) = drawNCard(0, deck)
    drawnCards shouldBe empty
    remainingDeck should be (deck)
  }

  it should "return an empty list when the number of card to draw is lower than 0" in {
    val deck = List(Card(Diamond, Jack()), Card(Spade, Ace()), Card(Heart, Queen()), Card(Club, Ace()))
    val (drawnCards, remainingDeck) = drawNCard(-1, deck)
    drawnCards shouldBe empty
    remainingDeck should be (deck)
  }

  "drawFirstCard" should "return the first card when the deck has more than one card" in {
    val firstCard = Card(Diamond, Jack())
    val deck = List(firstCard, Card(Spade, Ace()), Card(Heart, Queen()), Card(Club, Ace()))
    val (drawnCard, remainingDeck) = drawFirstCard(deck)
    drawnCard.value should be(firstCard)
    remainingDeck should contain inOrderOnly(Card(Spade, Ace()), Card(Heart, Queen()), Card(Club, Ace()))
  }

  it should "return the first card when the deck has only one card" in {
    val firstCard = Card(Diamond, Jack())
    val deck = List(firstCard)
    val (drawnCard, remainingDeck) = drawFirstCard(deck)
    drawnCard.value should be(firstCard)
    remainingDeck shouldBe empty
  }

  it should "return None when the deck is empty" in {
    val deck = Nil
    val (drawnCard, remainingDeck) = drawFirstCard(deck)
    drawnCard should be (None)
    remainingDeck should be (deck)
  }
}
