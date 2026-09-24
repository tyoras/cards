package io.tyoras.cards.tests

import cats.Eq
import io.circe.testing.ArbitraryInstances
import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.game.GameTyp.{Schnapsen, War}
import io.tyoras.cards.domain.game.GameType
import org.scalacheck.{Arbitrary, Gen}

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

trait CodecTestUtils extends ArbitraryInstances:
  given Arbitrary[Suit]      = Arbitrary(suitGen)
  given Eq[Suit]             = Eq.fromUniversalEquals
  given Arbitrary[Set[Suit]] = Arbitrary(suitsGen)
  given Arbitrary[Rank]      = Arbitrary(defaultRankGen)
  given Eq[Rank]             = Eq.fromUniversalEquals
  given Arbitrary[Deck]      = Arbitrary(randomDeckGen)
  given Arbitrary[Card]      = Arbitrary(cardGen)
  given Eq[GameType]         = Eq.fromUniversalEquals
  given Arbitrary[GameType]  = Arbitrary(Gen.oneOf(Schnapsen, War))
