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

  private def filtered(state: ERsGameState): PlayerGameState = state.filterForPlayer(p1)

  private def roundTrip(state: PlayerGameState) =
    val codec = PlayerGameState.given_GameStateFilter_ERsGameState.codec
    codec.decodeJson(codec(state)) shouldBe Right(state)

  "The filter" should "convert Init" in {
    val state = filtered(ERsGameState.Init(context, Set(p2)))
    state shouldBe PlayerGameState.Init(turn, p1, Set(p2), Set(p1))
    state.code shouldBe "init"
    state.label shouldBe "Initialisation"
    roundTrip(state)
  }

  private val pile = NonEmptyList.of(Card(Club, Five()), Card(Diamond, Six()))

  it should "convert PlayerTurn with an empty pile" in {
    val state = filtered(ERsGameState.PlayerTurn(context, p2))
    state shouldBe PlayerGameState.PlayerTurn(turn, p1, p2, None, Card.Count(0), Card.Count(1), Map(p1 -> Card.Count(1), p2 -> Card.Count(2)))
    state.code shouldBe "player-turn"
    state.label shouldBe s"Waiting for player [$p2] to play"
    roundTrip(state)
  }

  it should "convert PlayerTurn with a pile" in {
    val state = filtered(ERsGameState.PlayerTurn(context, p2, pile.toList))
    state shouldBe PlayerGameState.PlayerTurn(turn, p1, p2, Some(pile.head), Card.Count(2), Card.Count(1), Map(p1 -> Card.Count(1), p2 -> Card.Count(2)))
    roundTrip(state)
  }

  it should "convert ChallengeTurn" in {
    val state = filtered(ERsGameState.ChallengeTurn(context, p2, p1, 3, 1, pile))
    state shouldBe PlayerGameState.ChallengeTurn(turn, p1, p2, p1, 3, 1, pile.head, Card.Count(2), Card.Count(1), Map(p1 -> Card.Count(1), p2 -> Card.Count(2)))
    state.code shouldBe "challenge-turn"
    state.label shouldBe s"Challenge turn for player [$p2] against player [$p1] | Tries 2/3"
    roundTrip(state)
  }

  it should "convert SlapPenalty" in {
    val original = ERsGameState.PlayerTurn(context, p2, pile.toList)
    val state    = filtered(ERsGameState.SlapPenalty(context, p2, pile, original))
    state shouldBe PlayerGameState.SlapPenalty(
      turn = turn,
      playerId = p1,
      penalizedPlayer = p2,
      pileTopCard = pile.head,
      pileSize = Card.Count(2),
      handTopCard = Some(p1Card),
      handSize = Card.Count(1),
      opponents = Map(p1 -> Card.Count(1), p2 -> Card.Count(2)),
      discardedCards = Nil,
      remainingCardsToDiscard = Card.Count(1),
      acked = Set.empty,
      notAcked = Set(p1)
    )
    state.code shouldBe "slap-penalty"
    state.label shouldBe s"Player [$p2] has made an invalid slap"
    roundTrip(state)
  }

  it should "convert SlapPenalty once the penalty is payed" in {
    val original = ERsGameState.PlayerTurn(context, p2, pile.toList)
    filtered(ERsGameState.SlapPenalty(context, p2, pile, original, List(p2Card), Set(p1))) match
      case s: PlayerGameState.SlapPenalty =>
        s.discardedCards shouldBe List(p2Card)
        s.remainingCardsToDiscard shouldBe Card.Count(0)
        s.acked shouldBe Set(p1)
        s.notAcked shouldBe empty
        roundTrip(s)
      case s => fail(s"Expected SlapPenalty but got $s")
  }

  it should "convert PlayerWinTurn" in {
    val state = filtered(ERsGameState.PlayerWinTurn(context, p2, pile, Set(p1), Set(p2)))
    state shouldBe PlayerGameState.PlayerWinTurn(turn, p1, Card.Count(1), Map(p1 -> Card.Count(1), p2 -> Card.Count(2)), p2, pile, Set(p1), Set(p2), Set(p1))
    state.code shouldBe "player-win-turn"
    state.label shouldBe s"Player $p2 has won the turn and won 2 cards."
    roundTrip(state)
  }

  it should "convert Finish" in {
    val state = filtered(ERsGameState.Finish(context, p1))
    state shouldBe PlayerGameState.Finish(turn, p1, p1, context.eliminations)
    state.code shouldBe "finish"
    state.label shouldBe "Finish"
    roundTrip(state)
  }

  it should "convert Exit" in {
    val state = filtered(ERsGameState.Exit(context))
    state shouldBe PlayerGameState.Exit(turn, p1)
    state.code shouldBe "exit"
    state.label shouldBe "Exit"
    roundTrip(state)
  }
