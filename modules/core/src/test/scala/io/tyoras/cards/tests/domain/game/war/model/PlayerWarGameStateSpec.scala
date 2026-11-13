package io.tyoras.cards.tests.domain.game.war.model

import cats.data.{NonEmptyList, NonEmptySet}
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.{Card, Turn}
import io.tyoras.cards.domain.game.war.model.PlayerGameState.given
import io.tyoras.cards.domain.game.war.model.*
import io.tyoras.cards.domain.game.war.pickFirstCard
import io.tyoras.cards.tests.{pick, userIdGen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{ZoneOffset, ZonedDateTime}

class PlayerWarGameStateSpec extends AnyFlatSpec with Matchers:

  private val p1: PlayerId = userIdGen.pick
  private val p2: PlayerId = userIdGen.pick
  private val p1Card       = Card(Spade, Ace())
  private val p2Card       = Card(Heart, Two())
  private val turn         = Turn(3)

  private val context = GameContext(
    Map(p1 -> Player(p1, List(p1Card)), p2 -> Player(p2, List(p2Card, Card(Heart, Three())))),
    ZonedDateTime.of(2026, 1, 1, 12, 0, 0, 0, ZoneOffset.UTC),
    turn
  )
  private val counts = context.playersCardCount

  private def filtered(state: WarGameState): PlayerGameState = state.filterForPlayer(p1)

  private def roundTrip(state: PlayerGameState) =
    val codec = PlayerGameState.given_GameStateFilter_WarGameState.codec
    codec.decodeJson(codec(state)) shouldBe Right(state)

  "The filter" should "convert Init" in {
    val state = filtered(WarGameState.Init(context, Set(p2)))
    state shouldBe PlayerGameState.Init(turn, p1, Set(p2), Set(p1))
    state.code shouldBe "init"
    state.label shouldBe "Initialisation"
    roundTrip(state)
  }

  it should "convert BattleTurn showing only the hand of the player" in {
    val state = filtered(WarGameState.BattleTurn(context, Map(p2 -> Card(Club, Ten()))))
    state shouldBe PlayerGameState.BattleTurn(turn, p1, List(p1Card), counts, Map(p2 -> Card(Club, Ten())), Set(p1))
    state.code shouldBe "battle-turn"
    state.label should include(p1.toString)
    state.asInstanceOf[PlayerGameState.BattleTurn].pickFirstCard shouldBe Some(p1Card)
    roundTrip(state)
  }

  it should "convert WarTurn" in {
    val round = WarGameState.WarTurn.BattleRound(NonEmptySet.of(p1, p2), Map.empty, Map(p1 -> Card(Club, Ten()), p2 -> Card(Diamond, Ten())))
    val state = filtered(WarGameState.WarTurn(context, NonEmptyList.one(round)))
    val war   = state.asInstanceOf[PlayerGameState.WarTurn]
    war.hand shouldBe List(p1Card)
    war.opponents shouldBe counts
    war.battles.toList shouldBe List(PlayerGameState.WarTurn.BattleRound(round.involvedPlayers, round.hiddenPlayedCards, round.fightingCards))
    war.missingHidden shouldBe Set(p1, p2)
    war.missingPlays shouldBe Set(p1, p2)
    war.currentRound shouldBe war.battles.last
    war.pickFirstCard shouldBe Some(p1Card)
    state.code shouldBe "war-turn"
    state.label should startWith("War battle round 0 between players")
    roundTrip(state)
  }

  it should "convert PlayerWinTurn" in {
    val won   = Set(p1Card, p2Card)
    val state = filtered(WarGameState.PlayerWinTurn(context, p2, won, Set(p1), Set(p2)))
    state shouldBe PlayerGameState.PlayerWinTurn(turn, p1, List(p1Card), counts, p2, won, Set(p1), Set(p2), Set(p1))
    state.code shouldBe "player-win-turn"
    state.label shouldBe s"Player $p2 has won the turn and won 2 cards."
    roundTrip(state)
  }

  it should "convert Finish" in {
    val eliminated = context.eliminatePlayer(p2)
    val state      = filtered(WarGameState.Finish(eliminated, p1))
    state shouldBe PlayerGameState.Finish(turn, p1, p1, List(Elimination(p2, turn)))
    state.code shouldBe "finish"
    state.label shouldBe "Finish"
    roundTrip(state)
  }

  it should "convert Exit" in {
    val state = filtered(WarGameState.Exit(context))
    state shouldBe PlayerGameState.Exit(turn, p1)
    state.code shouldBe "exit"
    state.label shouldBe "Exit"
    roundTrip(state)
  }

  it should "give an empty hand to an unknown player" in {
    val state = WarGameState.BattleTurn(context).filterForPlayer(userIdGen.pick)
    state.asInstanceOf[PlayerGameState.BattleTurn].hand shouldBe empty
    state.asInstanceOf[PlayerGameState.BattleTurn].pickFirstCard shouldBe None
  }

  "GameState.pickFirstCard" should "return the first card of the player" in {
    (WarGameState.Init(context): WarGameState).pickFirstCard(p2) shouldBe Some(p2Card)
  }

  "WarError" should "expose its code and message" in {
    val errors = List[(WarError, String)](
      DeckError("deck")       -> "deck_error",
      WrongPlayer             -> "wrong_player",
      InvalidAction()         -> "invalid_action",
      InvalidCard()           -> "invalid_card",
      InvalidPlayer("player") -> "invalid_player",
      InvalidState("state")   -> "invalid_state"
    )
    errors.foreach { (error, code) =>
      error.code shouldBe code
      error.getMessage should not be empty
    }
  }
