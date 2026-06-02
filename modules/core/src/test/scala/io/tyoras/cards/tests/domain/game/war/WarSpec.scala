package io.tyoras.cards.tests.domain.game.war

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.{Heart, Spade}
import io.tyoras.cards.domain.card.{Card, Rank}
import io.tyoras.cards.domain.game.war.War
import io.tyoras.cards.domain.game.war.model.GameState.*
import io.tyoras.cards.domain.game.war.model.*
import io.tyoras.cards.domain.game.war.model.WarInput.GameInput.Ready
import io.tyoras.cards.domain.game.war.model.WarInput.{GameInput, MetaInput}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.util.UUID

class WarSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val playerIds   = NonEmptyList.of(FUUID.fromUUID(UUID.randomUUID()), FUUID.fromUUID(UUID.randomUUID()))
  // private val playerIds = NonEmptyList.of(FUUID.fromUUID(UUID.randomUUID()), FUUID.fromUUID(UUID.randomUUID()), FUUID.fromUUID(UUID.randomUUID()))

  "War.apply" should "initialize game with Init state" in {
    War[IO](playerIds).flatMap(_.currentState).asserting {
      case _: Init => succeed
      case state   => fail(s"Expected Init state but got $state")
    }
  }

  it should "distribute cards evenly among players" in {
    War[IO](playerIds).flatMap(_.currentState).asserting {
      case init: Init =>
        val handSizes = init.context.players.values.map(_.hand.size).toList
        handSizes shouldBe List(26, 26)
      case state => fail(s"Expected Init state but got $state")
    }
  }

  "War.submitInput" should "transition from Init to BattleTurn when all players ready" in {
    for {
      war   <- War[IO](playerIds)
      init  <- war.currentState
      _     <- war.submitInput(Ready(playerIds.head))
      state <- war.submitInput(Ready(playerIds.last))
    } yield state shouldBe a[BattleTurn]
  }

  it should "keep Init state when not all players ready" in {
    for {
      war   <- War[IO](playerIds)
      init  <- war.currentState
      state <- war.submitInput(Ready(playerIds.head))
    } yield state shouldBe a[Init]
  }

  it should "reject ready input from non-existing player in Init state" in {
    for {
      war      <- War[IO](playerIds)
      randomId <- FUUID.randomFUUID[IO]
      result   <- war.submitInput(Ready(randomId)).attempt
    } yield result.isLeft shouldBe true
  }

  it should "accept first card play in BattleTurn" in {
    for {
      war        <- War[IO](playerIds)
      init       <- war.currentState
      _          <- war.submitInput(Ready(playerIds.head))
      battleTurn <- war.submitInput(Ready(playerIds.last))
      context   = battleTurn.context
      firstCard = context.players(playerIds.head).hand.head
      state <- war.submitInput(GameInput.PlayCard(playerIds.head, firstCard.id))
    } yield state shouldBe a[BattleTurn]
  }

  it should "reject invalid card play in BattleTurn" in {
    for {
      war        <- War[IO](playerIds)
      init       <- war.currentState
      _          <- war.submitInput(Ready(playerIds.head))
      battleTurn <- war.submitInput(Ready(playerIds.last))
      invalidCard = Card(Spade, Ace())
      result <- war.submitInput(GameInput.PlayCard(playerIds.head, invalidCard.id)).attempt
    } yield result.isLeft shouldBe true
  }

  it should "reject card play from wrong player in BattleTurn" in {
    for {
      war        <- War[IO](playerIds)
      init       <- war.currentState
      _          <- war.submitInput(Ready(playerIds.head))
      battleTurn <- war.submitInput(Ready(playerIds.last))
      context   = battleTurn.context
      firstCard = context.players(playerIds.head).hand.head
      _      <- war.submitInput(GameInput.PlayCard(playerIds.head, firstCard.id))
      result <- war.submitInput(GameInput.PlayCard(playerIds.head, firstCard.id)).attempt
    } yield result.isLeft shouldBe true
  }

  it should "transition to either PlayerWinTurn or WarTurn when battle resolved depending on if there is a unique winner" in {
    for {
      war        <- War[IO](playerIds)
      init       <- war.currentState
      _          <- war.submitInput(Ready(playerIds.head))
      battleTurn <- war.currentState
      _          <- war.submitInput(Ready(playerIds.last))
      context = battleTurn.context
      card1   = context.players(playerIds.head).hand.head
      card2   = context.players(playerIds.last).hand.head
      _     <- war.submitInput(GameInput.PlayCard(playerIds.head, card1.id))
      state <- war.submitInput(GameInput.PlayCard(playerIds.last, card2.id))
    } yield if card1.value != card2.value then state shouldBe a[PlayerWinTurn] else state shouldBe a[WarTurn]
  }

  it should "accept ready input in PlayerWinTurn" in {
    for {
      war        <- War[IO](playerIds)
      init       <- war.currentState
      _          <- war.submitInput(Ready(playerIds.head))
      _          <- war.submitInput(Ready(playerIds.last))
      battleTurn <- war.currentState
      context = battleTurn.context
      card1   = context.players(playerIds.head).hand.head
      card2   = context.players(playerIds.last).hand.head
      _       <- war.submitInput(GameInput.PlayCard(playerIds.head, card1.id))
      winTurn <- war.submitInput(GameInput.PlayCard(playerIds.last, card2.id))
      result  <-
        if winTurn.isInstanceOf[PlayerWinTurn] then war.submitInput(Ready(playerIds.head))
        else IO.pure(winTurn)
    } yield if winTurn.isInstanceOf[PlayerWinTurn] then result shouldBe a[PlayerWinTurn] else succeed
  }

  it should "transition from PlayerWinTurn to BattleTurn when all players acked" in {
    for {
      war        <- War[IO](playerIds)
      init       <- war.currentState
      _          <- war.submitInput(Ready(playerIds.head))
      _          <- war.submitInput(Ready(playerIds.last))
      battleTurn <- war.currentState
      context = battleTurn.context
      card1   = context.players(playerIds.head).hand.head
      card2   = context.players(playerIds.last).hand.head
      _       <- war.submitInput(GameInput.PlayCard(playerIds.head, card1.id))
      winTurn <- war.submitInput(GameInput.PlayCard(playerIds.last, card2.id))
      result  <-
        if winTurn.isInstanceOf[PlayerWinTurn] then
          for {
            _     <- war.submitInput(Ready(playerIds.head))
            state <- war.submitInput(Ready(playerIds.last))
          } yield state
        else IO.pure(winTurn)
    } yield if winTurn.isInstanceOf[PlayerWinTurn] then result shouldBe a[BattleTurn] else succeed
  }

  it should "restart game when Restart input received" in {
    for {
      war  <- War[IO](playerIds)
      init <- war.currentState
      playerId = init.context.players.keys.head
      state <- war.submitInput(MetaInput.Restart(playerId))
    } yield {
      state shouldBe a[Init]
      state.asInstanceOf[Init].ready shouldBe empty
    }
  }

  it should "transition to Exit when End input received" in {
    for {
      war  <- War[IO](playerIds)
      init <- war.currentState
      playerId = init.context.players.keys.head
      state <- war.submitInput(MetaInput.End(playerId))
    } yield state shouldBe a[Exit]
  }

  it should "ignore wrong input in any state" in {
    for {
      war  <- War[IO](playerIds)
      init <- war.currentState
      playerId    = init.context.players.keys.head
      invalidCard = Card(Heart, King())
      _     <- war.submitInput(GameInput.PlayCard(playerId, invalidCard.id))
      state <- war.currentState
    } yield state shouldBe init
  }
}
