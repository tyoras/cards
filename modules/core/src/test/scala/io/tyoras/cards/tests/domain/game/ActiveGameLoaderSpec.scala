package io.tyoras.cards.tests.domain.game

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import io.tyoras.cards.domain.card.Turn
import io.tyoras.cards.domain.game.{ActiveGameLoader, GameTyp}
import cats.data.NonEmptyList
import io.tyoras.cards.domain.game.ers.model.ERsGameState
import io.tyoras.cards.domain.game.ers.model as ers
import io.tyoras.cards.domain.game.war.model.{GameContext, WarGameState, Player}
import io.tyoras.cards.tests.{gameIdGen, pick, userIdGen}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.time.ZonedDateTime

class ActiveGameLoaderSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "ActiveGameLoader" should "resume a War game from its state" in {
    val playerId = userIdGen.pick
    val gameId   = gameIdGen.pick
    val state    = WarGameState.BattleTurn(GameContext(Map(playerId -> Player(playerId)), ZonedDateTime.now(), Turn.firstTurn))
    for
      game    <- ActiveGameLoader.make[IO].loadFromState(gameId, GameTyp.War, state)
      current <- game.currentState
    yield
      game.gameId shouldBe gameId
      current shouldBe state
  }

  it should "resume an Egyptian Ratscrew game from its state" in {
    val p1     = userIdGen.pick
    val p2     = userIdGen.pick
    val gameId = gameIdGen.pick
    val ctx    = ers.GameContext(Map(p1 -> ers.Player(p1), p2 -> ers.Player(p2)), ZonedDateTime.now(), Turn.firstTurn, NonEmptyList.of(p1, p2))
    val state  = ERsGameState.PlayerTurn(ctx, p2)
    for
      game    <- ActiveGameLoader.make[IO].loadFromState(gameId, GameTyp.EgyptianRatscrew, state)
      current <- game.currentState
    yield
      game.gameId shouldBe gameId
      game.gameType shouldBe GameTyp.EgyptianRatscrew
      current shouldBe state
  }
