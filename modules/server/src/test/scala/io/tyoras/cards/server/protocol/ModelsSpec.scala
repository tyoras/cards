package io.tyoras.cards.server.protocol

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.tyoras.cards.domain.game.GameTyp
import io.tyoras.cards.domain.game.war.War
import io.tyoras.cards.domain.game.war.model.GameState
import io.tyoras.cards.server.protocol.chat.Room
import io.tyoras.cards.server.protocol.fixtures.*
import io.tyoras.cards.server.protocol.game.{Games, ProtocolError}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

class ModelsSpec extends AnyFunSpec with Matchers:
  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  describe("Room") {
    it("should accept a valid name") {
      Room("lobby").map(_.room).toOption shouldBe Some("lobby")
    }
    it("should reject invalid names") {
      Room("x").isInvalid shouldBe true
      Room("   ").isInvalid shouldBe true
      Room("x" * 51).isInvalid shouldBe true
    }
  }

  describe("ProtocolError") {
    it("should expose an error code and a meaningful message") {
      val (gameId, p1, p2) = (newGameId, newUserId, newUserId)
      ProtocolError.GameAlreadyFinished(gameId).code shouldBe "game_already_finished"
      ProtocolError.ActiveGameNotFound(gameId, GameTyp.War).code shouldBe "game_not_found"
      ProtocolError.PlayerDoesNotBelongToGame(p1, gameId, GameTyp.War).code shouldBe "player_does_not_belong_to_game"
      val illegal = ProtocolError.IllegalGameInput(p1, p2, gameId, GameTyp.War)
      illegal.code shouldBe "illegal_game_input"
      illegal.getMessage should (include(p1.toString).and(include(p2.toString)).and(include(gameId.toString)))
    }
  }

  describe("Games") {
    val (p1, p2)         = (newUserId, newUserId)
    val (gameA, gameB)   = (newGameId, newGameId)
    val List(warA, warB) =
      List(gameA, gameB).map(id => War.fromState[IO](id, GameState.Init(twoPlayersContext(p1, p2))).unsafeRunSync()): @unchecked
    val games = Games.empty[IO].upsert(warA).upsert(warB)

    it("should get an upserted game") {
      games.get(gameA, GameTyp.War) shouldBe Some(warA)
      games.get(gameA, GameTyp.Schnapsen) shouldBe None
    }
    it("should replace a game with the same id and type") {
      games.upsert(warA).activeGames should have size 2
    }
    it("should filter games by type") {
      games.filtered(GameTyp.War) should contain theSameElementsAs List(warA, warB)
      games.filtered(GameTyp.Schnapsen) shouldBe empty
    }
    it("should remove a game") {
      games.removeGame(gameA, GameTyp.War).activeGames.keySet shouldBe Set(gameB -> GameTyp.War)
    }
  }
