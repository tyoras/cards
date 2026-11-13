package io.tyoras.cards.tests.domain.game.ers.model

import io.tyoras.cards.domain.game.GameError
import io.tyoras.cards.domain.game.ers.model.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ERSErrorSpec extends AnyFlatSpec with Matchers:

  private def check(error: GameError, code: String, message: String) =
    error.code shouldBe code
    error.getMessage shouldBe message

  "ERS errors" should "expose their code and message" in {
    check(DeckError("deck"), "deck_error", "deck")
    check(WrongPlayer, "wrong_player", "Another player than the expected one has tried to play.")
    check(InvalidAction("action"), "invalid_action", "action")
    check(InvalidCard("card"), "invalid_card", "card")
    check(InvalidPlayer("player"), "invalid_player", "player")
    check(InvalidState("state"), "invalid_state", "state")
  }

  it should "have default messages for invalid action and invalid card" in {
    InvalidAction().getMessage shouldBe "The player has tried to play an invalid action."
    InvalidCard().getMessage shouldBe "The player has tried to play an invalid card."
  }
