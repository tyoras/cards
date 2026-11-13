package io.tyoras.cards.tests.domain.game.ers.model

import cats.data.NonEmptyList
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.{Card, Turn}
import io.tyoras.cards.domain.game.ers.model.*
import io.tyoras.cards.domain.game.ers.model.GameState.*
import io.tyoras.cards.tests.{pick, userIdGen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime

class GameStateSpec extends AnyFlatSpec with Matchers:

  private val p1: PlayerId = userIdGen.pick
  private val p2: PlayerId = userIdGen.pick
  private val p3: PlayerId = userIdGen.pick

  private val card1 = Card(Spade, Two())
  private val card2 = Card(Heart, Three())
  private val card3 = Card(Club, Four())

  private val context = GameContext(
    Map(p1 -> Player(p1, List(card1)), p2 -> Player(p2, List(card2)), p3 -> Player(p3, List(card3))),
    ZonedDateTime.now(),
    Turn.firstTurn,
    NonEmptyList.of(p1, p2, p3)
  )

  private val pile = NonEmptyList.of(Card(Diamond, Five()), Card(Diamond, Six()))

  "Init" should "expose code and label" in {
    val s = Init(context)
    s.code shouldBe "init"
    s.label shouldBe "Initialisation"
  }

  it should "compute the not ready players" in {
    Init(context).notReady shouldBe Set(p1, p2, p3)
    Init(context, Set(p1, p3)).notReady shouldBe Set(p2)
    Init(context, Set(p1, p2, p3)).notReady shouldBe empty
  }

  "PlayerTurn" should "expose code and label" in {
    val s = PlayerTurn(context, p1)
    s.code shouldBe "player-turn"
    s.label shouldBe s"Waiting for player [$p1] to play"
    s.pile shouldBe empty
  }

  it should "compute the next player" in {
    PlayerTurn(context, p1).nextPlayer shouldBe Some(p2)
  }

  "ChallengeTurn" should "expose code and label with the tries count" in {
    val s = ChallengeTurn(context, p1, p2, 3, 1, pile)
    s.code shouldBe "challenge-turn"
    s.label shouldBe s"Challenge turn for player [$p1] against player [$p2] | Tries 2/3"
  }

  "SlapPenalty" should "expose code and label" in {
    val s = SlapPenalty(context, p1, pile, PlayerTurn(context, p2))
    s.code shouldBe "slap-penalty"
    s.label shouldBe s"Player [$p1] has made an invalid slap"
  }

  it should "not expect an ack from the penalized player" in {
    SlapPenalty(context, p1, pile, PlayerTurn(context, p2)).notAcked shouldBe Set(p2, p3)
    SlapPenalty(context, p1, pile, PlayerTurn(context, p2), acked = Set(p2)).notAcked shouldBe Set(p3)
  }

  it should "not be payed while no card has been discarded" in {
    SlapPenalty(context, p1, pile, PlayerTurn(context, p2)).penaltyPayed shouldBe false
  }

  it should "be payed once enough cards have been discarded" in {
    SlapPenalty(context, p1, pile, PlayerTurn(context, p2), discardedCards = List(card1)).penaltyPayed shouldBe true
  }

  it should "be payed when the penalized player has no card left" in {
    val emptyHandCtx = context.updatePlayer(p1)(_.copy(hand = Nil))
    SlapPenalty(emptyHandCtx, p1, pile, PlayerTurn(context, p2)).penaltyPayed shouldBe true
  }

  "PlayerWinTurn" should "expose code and label" in {
    val s = PlayerWinTurn(context, p1, pile, Set.empty)
    s.code shouldBe "player-win-turn"
    s.label shouldBe s"Player $p1 has won the turn and won 2 cards."
  }

  it should "compute the not acked players" in {
    PlayerWinTurn(context, p1, pile, Set.empty).notAcked shouldBe Set(p1, p2, p3)
    PlayerWinTurn(context, p1, pile, Set.empty, Set(p1, p2)).notAcked shouldBe Set(p3)
  }

  "Finish" should "expose code, label and losers" in {
    val s = Finish(context, p2)
    s.code shouldBe "finish"
    s.label shouldBe "Finish"
    s.losers should contain theSameElementsAs List(p1, p3)
  }

  "Exit" should "expose code and label" in {
    val s = Exit(context)
    s.code shouldBe "exit"
    s.label shouldBe "Exit"
  }
