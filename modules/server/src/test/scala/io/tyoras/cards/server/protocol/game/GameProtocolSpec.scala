package io.tyoras.cards.server.protocol.game

import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import dev.profunktor.auth.jwt.JwtToken
import io.tyoras.cards.domain.game.war.War
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.domain.game.war.model.GameState
import io.tyoras.cards.domain.game.war.model.WarInput.GameInput.Ready
import io.tyoras.cards.domain.game.{Game, GameTyp}
import io.tyoras.cards.server.endpoints.utils.{TestAuthService, TestData}
import io.tyoras.cards.server.protocol.fixtures.*
import io.tyoras.cards.shared.protocol.game.OutputMessage
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

class GameProtocolSpec extends AnyFunSpec with Matchers:
  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val p1 = TestData.existingUser.id
  private val p2 = newUserId

  private case class Env(protocol: GameProtocol[IO], games: InMemoryGameService, stats: RecordingGameStatService)

  /** Runs the test with a protocol whose persistence contains the given games. The protocol resource is released at the end of the test. */
  private def withProtocol[A](storedGames: Game.Existing[?]*)(test: Env => IO[A]): A =
    (for
      games  <- InMemoryGameService.make(storedGames*)
      stats  <- RecordingGameStatService.make
      result <- GameProtocol.make[IO](new TestAuthService, games, stats).use(p => test(Env(p, games, stats)))
    yield result).unsafeRunSync()

  private def storedState(env: Env, gameId: Game.ID): IO[Option[Game.Existing[GameState]]] =
    env.games.readById[GameState](gameId)

  describe("connectPlayer") {
    it("should connect a player of a stored game and load it as an active game") {
      val gameId             = newGameId
      val (output, isActive) = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2)))) { env =>
        for
          out    <- env.protocol.connectPlayer(gameId, GameTyp.War, TestData.existingUser)
          active <- env.protocol.activeGames
        yield out -> active.get(gameId, GameTyp.War).isDefined
      }
      output shouldBe OutputMessage.PlayerConnectionSuccess(gameId, GameTyp.War, p1, TestData.existingUser.name)
      isActive shouldBe true
    }

    it("should reject a player who does not belong to the game") {
      val gameId = newGameId
      val output = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(newUserId, p2)))) {
        _.protocol.connectPlayer(gameId, GameTyp.War, TestData.existingUser)
      }
      output shouldBe OutputMessage.AuthError("player_does_not_belong_to_game", ProtocolError.PlayerDoesNotBelongToGame(p1, gameId, GameTyp.War).getMessage)
    }

    it("should reject a connection to an unknown game") {
      val gameId = newGameId
      val output = withProtocol()(_.protocol.connectPlayer(gameId, GameTyp.War, TestData.existingUser))
      output shouldBe OutputMessage.AuthError("game_not_found", ProtocolError.ActiveGameNotFound(gameId, GameTyp.War).getMessage)
    }

    it("should reject on a finished game") {
      val gameId = newGameId
      val output = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2)), finishedAt = Some(now))) {
        _.protocol.authPlayer(gameId, GameTyp.War, TestData.existingUserAuthToken)
      }
      output shouldBe OutputMessage.AuthError("game_already_finished", s"Game with id $gameId is already finished")
    }
  }

  describe("authPlayer") {
    it("should connect the player owning a valid token") {
      val gameId = newGameId
      val output = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2)))) {
        _.protocol.authPlayer(gameId, GameTyp.War, TestData.existingUserAuthToken)
      }
      output shouldBe a[OutputMessage.PlayerConnectionSuccess]
    }

    it("should reject an invalid token") {
      val gameId = newGameId
      val output = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2)))) {
        _.protocol.authPlayer(gameId, GameTyp.War, JwtToken("invalid"))
      }
      output shouldBe OutputMessage.AuthError("token_auth", "Invalid token: unknown token")
    }

    it("should reject on a finished game") {
      val gameId = newGameId
      val output = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2)), finishedAt = Some(now))) {
        _.protocol.authPlayer(gameId, GameTyp.War, TestData.existingUserAuthToken)
      }
      output shouldBe OutputMessage.AuthError("game_already_finished", s"Game with id $gameId is already finished")
    }
  }

  describe("registerActiveGame") {
    it("should make the game active") {
      val gameId = newGameId
      // the game is also stored as the protocol saves all the active games on release
      val active = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2)))) { env =>
        War.fromState[IO](gameId, GameState.Init(twoPlayersContext(p1, p2))).flatMap(env.protocol.registerActiveGame(gameId, _)) >>
          env.protocol.activeGames
      }
      active.activeGames.keySet shouldBe Set(gameId -> GameTyp.War)
    }
  }

  describe("currentState") {
    it("should return the game state filtered for the player") {
      val gameId = newGameId
      val output = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2))))(_.protocol.currentState(gameId, GameTyp.War, p1))
      output match
        case OutputMessage.GameState(id, recipient, state) =>
          id shouldBe gameId
          recipient shouldBe p1
          state.hcursor.get[String]("player_id") shouldBe Right(p1.show)
        case other => fail(s"Unexpected output $other")
    }

    it("should return a protocol error for an unknown game") {
      val gameId = newGameId
      withProtocol()(_.protocol.currentState(gameId, GameTyp.War, p1)) shouldBe
        OutputMessage.ProtocolError(gameId, p1, "game_not_found", ProtocolError.ActiveGameNotFound(gameId, GameTyp.War).getMessage)
    }

    it("should return a game error for any other failure") {
      val gameId = newGameId
      val output = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2)), finishedAt = Some(now))) {
        _.protocol.currentState(gameId, GameTyp.War, p1)
      }
      output shouldBe OutputMessage.GameError(gameId, p1, "state_failure", ProtocolError.GameAlreadyFinished(gameId).getMessage)
    }
  }

  describe("submitInput") {
    it("should apply the input and send the new state to every player") {
      val gameId          = newGameId
      val (output, state) = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2)))) { env =>
        for
          out   <- env.protocol.submitInput(gameId, GameTyp.War, p1, Ready(p1))
          game  <- env.protocol.activeGames.map(_.get(gameId, GameTyp.War).get)
          state <- game.currentState
        yield out -> state
      }
      output.collect { case OutputMessage.GameState(`gameId`, recipient, _) => recipient } should contain theSameElementsAs List(p1, p2)
      state shouldBe GameState.Init(twoPlayersContext(p1, p2), ready = Set(p1))
    }

    it("should reject an input submitted on behalf of another player") {
      val gameId = newGameId
      val output = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2)))) {
        _.protocol.submitInput(gameId, GameTyp.War, p1, Ready(p2))
      }
      output shouldBe List(
        OutputMessage.ProtocolError(gameId, p1, "illegal_game_input", ProtocolError.IllegalGameInput(p1, p2, gameId, GameTyp.War).getMessage)
      )
    }

    it("should return a protocol error for an unknown game") {
      val gameId = newGameId
      withProtocol()(_.protocol.submitInput(gameId, GameTyp.War, p1, Ready(p1))) shouldBe List(
        OutputMessage.ProtocolError(gameId, p1, "game_not_found", ProtocolError.ActiveGameNotFound(gameId, GameTyp.War).getMessage)
      )
    }

    it("should return a game error when the game rejects the input") {
      val gameId = newGameId
      val state  = GameState.Init(twoPlayersContext(p1, p2), ready = Set(p1))
      val output = withProtocol(warGame(gameId, state))(_.protocol.submitInput(gameId, GameTyp.War, p1, Ready(p1)))
      output.loneElement match
        case OutputMessage.GameError(`gameId`, `p1`, code, _) => code shouldBe "input_submission_failure"
        case other                                            => fail(s"Unexpected output $other")
    }

    it("should end the game, persist it and update the players stats when the game is finished") {
      val gameId                        = newGameId
      val (output, stored, active, upd) = withProtocol(warGame(gameId, almostFinishedState(p1, p2))) { env =>
        for
          out    <- env.protocol.submitInput(gameId, GameTyp.War, p1, Ready(p1))
          stored <- storedState(env, gameId)
          active <- env.protocol.activeGames
          upd    <- env.stats.updates.get
        yield (out, stored, active, upd)
      }
      output.last shouldBe OutputMessage.GameEnded(gameId)
      stored.flatMap(_.data.finishedAt) shouldBe defined
      stored.map(_.state) shouldBe Some(GameState.Finish(almostFinishedState(p1, p2).context, p1))
      active.activeGames shouldBe empty
      upd shouldBe List(StatUpdate(GameTyp.War, winners = List(p1), draws = Nil, losers = List(p2)))
    }
  }

  describe("saveGame") {
    it("should persist the current state of the active game") {
      val gameId = newGameId
      val stored = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2)))) { env =>
        env.protocol.submitInput(gameId, GameTyp.War, p1, Ready(p1)) >> env.protocol.saveGame(gameId, GameTyp.War) >> storedState(env, gameId)
      }
      stored.map(_.state) shouldBe Some(GameState.Init(twoPlayersContext(p1, p2), ready = Set(p1)))
    }

    it("should persist the active games when the protocol is released") {
      val gameId = newGameId
      val stored = (for
        games <- InMemoryGameService.make(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2))))
        stats <- RecordingGameStatService.make
        _     <- GameProtocol.make[IO](new TestAuthService, games, stats).use(_.submitInput(gameId, GameTyp.War, p1, Ready(p1)))
        state <- games.readById[GameState](gameId)
      yield state).unsafeRunSync()
      stored.map(_.state) shouldBe Some(GameState.Init(twoPlayersContext(p1, p2), ready = Set(p1)))
    }
  }

  describe("endGame") {
    it("should finish the game without updating the stats when it was exited") {
      val gameId                  = newGameId
      val exit                    = GameState.Exit(twoPlayersContext(p1, p2))
      val (output, stored, stats) = withProtocol(warGame(gameId, exit)) { env =>
        (env.protocol.endGame(gameId, GameTyp.War), storedState(env, gameId), env.stats.updates.get).tupled
      }
      output shouldBe List(OutputMessage.GameEnded(gameId))
      stored.flatMap(_.data.finishedAt) shouldBe defined
      stats shouldBe empty
    }

    it("should fail for an unknown game") {
      withProtocol()(_.protocol.endGame(newGameId, GameTyp.War).attempt).left.toOption.get shouldBe a[ProtocolError.ActiveGameNotFound]
    }
  }

  describe("disconnect") {
    it("should disconnect the player and persist the game state") {
      val gameId                     = newGameId
      val (output, playerRef, saved) = withProtocol(warGame(gameId, GameState.Init(twoPlayersContext(p1, p2)))) { env =>
        for
          ref   <- Ref.of[IO, Option[ConnectedPlayer]](Some(ConnectedPlayer(gameId, GameTyp.War, p1, "test")))
          _     <- env.protocol.submitInput(gameId, GameTyp.War, p1, Ready(p1))
          out   <- env.protocol.disconnect(ref)
          after <- ref.get
          saved <- storedState(env, gameId)
        yield (out, after, saved)
      }
      output shouldBe OutputMessage.PlayerDisconnected(gameId, p1, "test")
      playerRef shouldBe None
      saved.map(_.state) shouldBe Some(GameState.Init(twoPlayersContext(p1, p2), ready = Set(p1)))
    }

    it("should discard the disconnection of an already disconnected player") {
      withProtocol()(env => Ref.of[IO, Option[ConnectedPlayer]](None).flatMap(env.protocol.disconnect)) shouldBe OutputMessage.DiscardMessage
    }
  }

  extension [A](l: List[A])
    private def loneElement: A =
      l should have size 1
      l.head
