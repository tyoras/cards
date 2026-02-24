package io.tyoras.cards.tests.domain.game.war.model

import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.SyncIO
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.{Club, Diamond, Heart, Spade}
import io.tyoras.cards.domain.card.{Card, Hand}
import io.tyoras.cards.domain.game.war.model.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime

class GameStateSpec extends AnyFlatSpec with Matchers {

  val pId1: PlayerId = FUUID.randomFUUID[SyncIO].unsafeRunSync()
  val pId2: PlayerId = FUUID.randomFUUID[SyncIO].unsafeRunSync()
  val pId3: PlayerId = FUUID.randomFUUID[SyncIO].unsafeRunSync()

  val p1c1: Card = Card(Club, Ten())
  val p1c2: Card = Card(Heart, King())
  val p2c1: Card = Card(Spade, Ace())
  val card: Card = Card(Diamond, Five())

  val player1: Player             = Player(pId1, List(p1c1, p1c2))
  val player2: Player             = Player(pId2, List(p2c1))
  val playerWithEmptyHand: Player = Player(pId3, Hand.empty)

  val context: GameContext = GameContext(Map(pId1 -> player1, pId2 -> player2), ZonedDateTime.now(), Turn.firstTurn)
  val firstWarRound        = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map.empty, Map(pId1 -> Card(Club, Two()), pId2 -> Card(Heart, Two())))

  "Init.notReady" should "return all players when no one is ready" in {
    val init = GameState.Init(context)
    init.notReady shouldBe Set(pId1, pId2)
  }

  it should "return empty set when all players are ready" in {
    val init = GameState.Init(context, Set(pId1, pId2))
    init.notReady shouldBe Set.empty
  }

  it should "return only non-ready players" in {
    val init = GameState.Init(context, Set(pId1))
    init.notReady shouldBe Set(pId2)
  }

  "BattleTurn.missingPlays" should "return all non-eliminated players when no cards played" in {
    val battle = GameState.BattleTurn(context)
    battle.missingPlays shouldBe Set(pId1, pId2)
  }

  it should "return empty set when all non-eliminated players have played" in {
    val battle = GameState.BattleTurn(context, playedCards = Map(pId1 -> p1c1, pId2 -> p1c2))
    battle.missingPlays shouldBe Set.empty
  }

  it should "exclude eliminated players" in {
    val contextWithEliminated = context.copy(players = Map(pId1 -> player1, pId3 -> playerWithEmptyHand))
    val battle                = GameState.BattleTurn(contextWithEliminated, Map(pId1 -> p1c1))
    battle.missingPlays shouldBe Set.empty
  }

  it should "return players who have not played yet" in {
    val battle = GameState.BattleTurn(context, Map(pId1 -> p1c1))
    battle.missingPlays shouldBe Set(pId2)
  }

  "WarTurn.missingHidden" should "return players who have not played hidden cards" in {
    val round = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map.empty, Map.empty)
    val war   = GameState.WarTurn(context, NonEmptyList.of(firstWarRound, round))
    war.missingHidden shouldBe Set(pId1, pId2)
  }

  it should "return empty set when all players have played hidden cards" in {
    val round = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map(pId1 -> p1c1, pId2 -> p1c2), Map.empty)
    val war   = GameState.WarTurn(context, NonEmptyList.of(firstWarRound, round))
    war.missingHidden shouldBe Set.empty
  }

  it should "exclude eliminated players" in {
    val contextWithEliminated = context.copy(players = Map(pId1 -> player1, pId3 -> playerWithEmptyHand))
    val round                 = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId3), Map.empty, Map.empty)
    val war                   = GameState.WarTurn(contextWithEliminated, NonEmptyList.of(firstWarRound, round))
    war.missingHidden shouldBe Set(pId1)
  }

  "WarTurn.missingFighting" should "return players who have not played fighting cards" in {
    val round = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map(pId1 -> p1c1, pId2 -> p1c2), Map.empty)
    val war   = GameState.WarTurn(context, NonEmptyList.of(firstWarRound, round))
    war.missingFighting shouldBe Set(pId1, pId2)
  }

  it should "return empty set when all players have played fighting cards" in {
    val round = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map.empty, Map(pId1 -> p1c1, pId2 -> p1c2))
    val war   = GameState.WarTurn(context, NonEmptyList.of(firstWarRound, round))
    war.missingFighting shouldBe Set.empty
  }

  "WarTurn.allCardPlayed" should "return true when all cards are played" in {
    val round = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map(pId1 -> p1c1, pId2 -> p1c2), Map(pId1 -> p2c1, pId2 -> card))
    val war   = GameState.WarTurn(context, NonEmptyList.of(firstWarRound, round))
    war.allCardPlayed shouldBe true
  }

  it should "return false when some cards are missing" in {
    val round = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map(pId1 -> p1c1), Map.empty)
    val war   = GameState.WarTurn(context, NonEmptyList.of(firstWarRound, round))
    war.allCardPlayed shouldBe false
  }

  "WarTurn.heap" should "contain all cards from all rounds" in {
    val round1 = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map(pId1 -> p1c1), Map(pId1 -> p1c2))
    val round2 = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map(pId2 -> p2c1), Map(pId2 -> card))
    val war    = GameState.WarTurn(context, NonEmptyList.of(round1, round2))
    war.heap shouldBe Set(p1c1, p1c2, p2c1, card)
  }

  "WarTurn.playCard" should "add fighting card when hidden card already played" in {
    val round   = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map(pId1 -> p1c1), Map.empty)
    val war     = GameState.WarTurn(context, NonEmptyList.of(firstWarRound, round))
    val updated = war.playCard(pId1, p1c2)
    updated.currentRound.fightingCards shouldBe Map(pId1 -> p1c2)
  }

  it should "add hidden card when hidden card not played yet" in {
    val round   = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map.empty, Map.empty)
    val war     = GameState.WarTurn(context, NonEmptyList.of(firstWarRound, round))
    val updated = war.playCard(pId1, p1c1)
    updated.currentRound.hiddenPlayedCards shouldBe Map(pId1 -> p1c1)
  }

  it should "update only last round" in {
    val round1  = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map(pId1 -> p1c1), Map(pId1 -> p1c2))
    val round2  = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map.empty, Map.empty)
    val war     = GameState.WarTurn(context, NonEmptyList.of(round1, round2))
    val updated = war.playCard(pId1, p2c1)
    updated.battles.head shouldBe round1
    updated.currentRound.hiddenPlayedCards shouldBe Map(pId1 -> p2c1)
  }

  "BattleRound.heap" should "contain all hidden and fighting cards" in {
    val round = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1, pId2), Map(pId1 -> p1c1, pId2 -> p1c2), Map(pId1 -> p2c1, pId2 -> card))
    round.heap shouldBe Set(p1c1, p1c2, p2c1, card)
  }

  it should "contain only hidden cards when no fighting cards" in {
    val round = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1), Map(pId1 -> p1c1), Map.empty)
    round.heap shouldBe Set(p1c1)
  }

  it should "be empty when no cards played" in {
    val round = GameState.WarTurn.BattleRound(NonEmptySet.of(pId1), Map.empty, Map.empty)
    round.heap shouldBe Set.empty
  }

  "PlayerWinTurn.notAcked" should "return all players when no one acknowledged" in {
    val win = GameState.PlayerWinTurn(context, pId1, Set(p1c1), Set.empty)
    win.notAcked shouldBe Set(pId1, pId2)
  }

  it should "return empty set when all players acknowledged" in {
    val win = GameState.PlayerWinTurn(context, pId1, Set(p1c1), Set.empty, Set(pId1, pId2))
    win.notAcked shouldBe Set.empty
  }

  it should "return only players who have not acknowledged" in {
    val win = GameState.PlayerWinTurn(context, pId1, Set(p1c1), Set.empty, Set(pId1))
    win.notAcked shouldBe Set(pId2)
  }
}
