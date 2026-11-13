package io.tyoras.cards.tests.domain.game

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import io.tyoras.cards.domain.card.Turn
import io.tyoras.cards.domain.game.{ActiveGameLoader, GameTyp}
import io.tyoras.cards.domain.game.war.model.{GameContext, GameState, Player}
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
    val state    = GameState.BattleTurn(GameContext(Map(playerId -> Player(playerId)), ZonedDateTime.now(), Turn.firstTurn))
    for
      game    <- ActiveGameLoader.make[IO].loadFromState(gameId, GameTyp.War, state)
      current <- game.currentState
    yield
      game.gameId shouldBe gameId
      current shouldBe state
  }
