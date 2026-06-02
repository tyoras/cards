package io.tyoras.cards.tests

import org.scalacheck.Gen
import io.tyoras.cards.domain.card.*

val suitGen: Gen[Suit]       = Gen.oneOf(allSuits)
val suitsGen: Gen[Set[Suit]] = Gen.containerOf[Set, Suit](suitGen)

val defaultRankGen: Gen[Rank]       = Gen.oneOf(defaultRanks)
val defaultRanksGen: Gen[Set[Rank]] = Gen.containerOf[Set, Rank](defaultRankGen)

val international52DeckGen: Gen[Deck] = Gen.delay(international52Deck.shuffled)
val randomDeckGen: Gen[Deck]          = for
  suits <- suitsGen
  ranks <- defaultRanksGen
  sortedDeck = Deck.create(suits, ranks)
  shuffledDeck <- Gen.delay(sortedDeck.shuffled)
yield shuffledDeck

val cardGen: Gen[Card] = for
  suit <- suitGen
  rank <- defaultRankGen
yield Card(suit, rank)
