package io.tyoras.cards

import org.scalacheck.Gen
import io.tyoras.cards.domain.card._

package object tests {

  val suitGen: Gen[Suit] = Gen.oneOf(allSuits)
  val suitsGen: Gen[Set[Suit]] = Gen.containerOf[Set, Suit](suitGen)

  val defaultRankGen: Gen[Rank] = Gen.oneOf(defaultRanks)
  val defaultRanksGen: Gen[Set[Rank]] = Gen.containerOf[Set, Rank](defaultRankGen)

  val international52DeckGen: Gen[Deck] = Gen.delay(shuffle(international52Deck))
  val randomDeckGen: Gen[Deck] = for {
    suits <- suitsGen
    ranks <- defaultRanksGen
    sortedDeck = createDeck(suits, ranks)
    shuffledDeck <- Gen.delay(shuffle(sortedDeck))
  } yield shuffledDeck

  val cardGen: Gen[Card] = for {
    suit <- suitGen
    rank <- defaultRankGen
  } yield Card(suit, rank)
}
