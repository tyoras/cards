package io.tyoras.cards.tests.domain.game.war.model

import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.card.Rank.Ace
import io.tyoras.cards.domain.card.Suit.Spade
import io.tyoras.cards.domain.game.war.model.*
import io.tyoras.cards.domain.game.war.model.WarInput.GameInput.{PlayCard, Ready}
import io.tyoras.cards.domain.game.war.model.WarInput.MetaInput.{End, Restart}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WarInputSpec extends AnyFlatSpec with Matchers {

  val playerId: PlayerId = FUUID.randomFUUID[cats.effect.SyncIO].unsafeRunSync()
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

  "GameInput.PlayCard" should "have correct label with card and player id" in {
    val playCard = PlayCard(playerId, card)
    playCard.label should include("Play card")
    playCard.label should include(card.toString)
    playCard.label should include(playerId.toString)
  }

  it should "return player id" in {
    val playCard = PlayCard(playerId, card)
    playCard.playerId shouldBe playerId
  }

  it should "return card" in {
    val playCard: PlayCard = PlayCard(playerId, card)
    playCard.card shouldBe card
  }

  it should "return label as toString" in {
    val playCard = PlayCard(playerId, card)
    playCard.toString shouldBe playCard.label
  }
}
