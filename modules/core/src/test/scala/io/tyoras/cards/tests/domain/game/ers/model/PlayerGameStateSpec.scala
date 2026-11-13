package io.tyoras.cards.tests.domain.game.ers.model

import cats.data.NonEmptyList
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.{Card, Turn}
import io.tyoras.cards.domain.game.ers.model.PlayerGameState.given
import io.tyoras.cards.domain.game.ers.model.*
import io.tyoras.cards.tests.{pick, userIdGen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{ZoneOffset, ZonedDateTime}

class PlayerGameStateSpec extends AnyFlatSpec with Matchers:

  private val p1: PlayerId = userIdGen.pick
  private val p2: PlayerId = userIdGen.pick
  private val p1Card       = Card(Spade, Ace())
  private val p2Card       = Card(Heart, Two())
  private val turn         = Turn(3)

  private val context = GameContext(
    Map(p1 -> Player(p1, List(p1Card)), p2 -> Player(p2, List(p2Card, Card(Heart, Three())))),
    ZonedDateTime.of(2026, 1, 1, 12, 0, 0, 0, ZoneOffset.UTC),
    turn,
    NonEmptyList.of(p1, p2),
    List(Elimination(p2, Turn(2)))
  )

  private def filtered(state: GameState): PlayerGameState = state.filterForPlayer(p1)

  private def roundTrip(state: PlayerGameState) =
    val codec = PlayerGameState.given_GameStateFilter_GameState.codec
    codec.decodeJson(codec(state)) shouldBe Right(state)

  "The filter" should "convert Init" in {
    val state = filtered(GameState.Init(context, Set(p2)))
    state shouldBe PlayerGameState.Init(turn, p1, Set(p2), Set(p1))
    state.code shouldBe "init"
    state.label shouldBe "Initialisation"
    roundTrip(state)
  }

  it should "convert PlayerWinTurn showing only the hand of the player" in {
    val wonCards = NonEmptyList.of(Card(Club, Ten()), Card(Club, Nine()))
    val state    = filtered(GameState.PlayerWinTurn(context, p2, wonCards, Set.empty, Set(p2)))
    state shouldBe PlayerGameState.PlayerWinTurn(
      turn,
      p1,
      List(p1Card),
      context.playersCardCount,
      p2,
      wonCards,
      Set.empty,
      Set(p2),
      Set(p1)
    )
    state.code shouldBe "player-win-turn"
    state.label shouldBe s"Player $p2 has won the turn and won 2 cards."
    roundTrip(state)
  }

  it should "convert Finish" in {
    val state = filtered(GameState.Finish(context, p1))
    state shouldBe PlayerGameState.Finish(turn, p1, p1, context.eliminations)
    state.code shouldBe "finish"
    state.label shouldBe "Finish"
    roundTrip(state)
  }

  it should "convert Exit" in {
    val state = filtered(GameState.Exit(context))
    state shouldBe PlayerGameState.Exit(turn, p1)
    state.code shouldBe "exit"
    state.label shouldBe "Exit"
    roundTrip(state)
  }
