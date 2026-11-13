package io.tyoras.cards.tests.domain.game.ers

import cats.data.NonEmptyList
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.{Card, Turn}
import io.tyoras.cards.domain.game.ers.*
import io.tyoras.cards.domain.game.ers.model.*
import io.tyoras.cards.domain.game.ers.model.GameState.Init
import io.tyoras.cards.tests.{pick, userIdGen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime

class ErsSpec extends AnyFlatSpec with Matchers:

  "ersDeck" should "be a 52 cards deck without duplicates" in {
    ersDeck should have size 52
    ersDeck.distinct should have size 52
  }

  "headCardChallenge" should "give the number of tries for each head card" in {
    headCardChallenge shouldBe Map(Ace() -> 4, King() -> 3, Queen() -> 2, Jack() -> 1)
  }

  "penalty" should "be one card" in {
    penalty shouldBe 1
  }

  "GameState.pickFirstCard" should "return the first card of the player hand" in {
    val p1: PlayerId = userIdGen.pick
    val p2: PlayerId = userIdGen.pick
    val card         = Card(Heart, Seven())
    val context      = GameContext(Map(p1 -> Player(p1, List(card)), p2 -> Player(p2)), ZonedDateTime.now(), Turn.firstTurn, NonEmptyList.of(p1, p2))
    val state        = Init(context)
    state.pickFirstCard(p1) shouldBe Some(card)
    state.pickFirstCard(p2) shouldBe None
  }
