package io.tyoras.cards.tests.domain.game.ers.model

import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.card.Rank.Ace
import io.tyoras.cards.domain.card.Suit.Spade
import io.tyoras.cards.domain.game.ers.model.*
import io.tyoras.cards.domain.game.ers.model.ERsInput.GameInput.{Discard, PlayCard, Ready, Slap}
import io.tyoras.cards.domain.game.ers.model.ERsInput.MetaInput.{End, Restart}
import io.tyoras.cards.tests.{pick, userIdGen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ERsInputSpec extends AnyFlatSpec with Matchers {

  val playerId: PlayerId = userIdGen.pick
  val card: Card         = Card(Spade, Ace())

  "GameInput.Ready" should "have correct label with player id" in {
    val ready = Ready(playerId)
    ready.label should include("Ready to start next turn")
    ready.label should include(playerId.toString)
  }

  it should "return player id" in {
    val ready = Ready(playerId)
    ready.playerId shouldBe playerId
  }

  it should "return label as toString" in {
    val ready = Ready(playerId)
    ready.toString shouldBe ready.label
  }

  "GameInput.Slap" should "have correct label with player id" in {
    val slap = Slap(playerId)
    slap.label should include("Slap heap")
    slap.label should include(playerId.toString)
  }

  it should "return player id" in {
    val slap = Slap(playerId)
    slap.playerId shouldBe playerId
  }

  it should "return label as toString" in {
    val slap = Slap(playerId)
    slap.toString shouldBe slap.label
  }

  "MetaInput.Restart" should "have correct label with player id" in {
    val restart = Restart(playerId)
    restart.label should include("Restart game")
    restart.label should include(playerId.toString)
  }

  it should "return player id" in {
    val restart = Restart(playerId)
    restart.playerId shouldBe playerId
  }

  it should "return label as toString" in {
    val restart = Restart(playerId)
    restart.toString shouldBe restart.label
  }

  "MetaInput.End" should "have correct label with player id" in {
    val end = End(playerId)
    end.label should include("Quit game")
    end.label should include(playerId.toString)
  }

  it should "return player id" in {
    val end = End(playerId)
    end.playerId shouldBe playerId
  }

  it should "return label as toString" in {
    val end = End(playerId)
    end.toString shouldBe end.label
  }

  "GameInput.PlayCard" should "have correct label with player id" in {
    val playCard = PlayCard(playerId)
    playCard.label should include("Play next card")
    playCard.label should include(playerId.toString)
  }

  it should "return player id" in {
    PlayCard(playerId).playerId shouldBe playerId
  }

  it should "return label as toString" in {
    val playCard = PlayCard(playerId)
    playCard.toString shouldBe playCard.label
  }

  "GameInput.Discard" should "have correct label with card and player ids" in {
    val discard = Discard(playerId, card.id)
    discard.label should include("Discard card")
    discard.label should include(card.id.toString)
    discard.label should include(playerId.toString)
  }

  it should "return player id and card id" in {
    val discard: Discard = Discard(playerId, card.id)
    discard.playerId shouldBe playerId
    discard.cardId shouldBe card.id
  }

  it should "return label as toString" in {
    val discard = Discard(playerId, card.id)
    discard.toString shouldBe discard.label
  }
}
