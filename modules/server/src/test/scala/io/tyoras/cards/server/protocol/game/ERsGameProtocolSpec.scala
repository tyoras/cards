package io.tyoras.cards.server.protocol.game

import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import io.circe.syntax.*
import io.tyoras.cards.domain.game.ers.codecs.given
import io.tyoras.cards.domain.game.ers.model.{ERsGameState, ERsInput}
import io.tyoras.cards.domain.game.ers.model.ERsInput.GameInput.{Discard, PlayCard, Ready}
import io.tyoras.cards.domain.game.{Game, GameTyp}
import io.tyoras.cards.server.endpoints.utils.{TestAuthService, TestData}
import io.tyoras.cards.server.protocol.fixtures.*
import io.tyoras.cards.shared.protocol.game.Commands.AuthenticatedCommand
import io.tyoras.cards.shared.protocol.game.Commands.AuthenticatedCommand.GameCommand
import io.tyoras.cards.shared.protocol.game.OutputMessage
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

/** Game protocol and input parser behaviour specific to the Egyptian Ratscrew game */
class ERsGameProtocolSpec extends AnyFunSpec with Matchers:
  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val p1       = TestData.existingUser.id
  private val p2       = newUserId
  private val ers      = GameTyp.EgyptianRatscrew
  private val initCtx  = ersTwoPlayersContext(p1, p2)
  private val initGame = (id: Game.ID) => ersGame(id, ERsGameState.Init(initCtx))

  private case class Env(protocol: GameProtocol[IO], games: InMemoryGameService, stats: RecordingGameStatService)

  private def withProtocol[A](storedGames: Game.Existing[?]*)(test: Env => IO[A]): A =
    (for
      games  <- InMemoryGameService.make(storedGames*)
      stats  <- RecordingGameStatService.make
      result <- GameProtocol.make[IO](new TestAuthService, games, stats).use(p => test(Env(p, games, stats)))
    yield result).unsafeRunSync()

  private def storedState(env: Env, gameId: Game.ID): IO[Option[Game.Existing[ERsGameState]]] =
    env.games.readById[ERsGameState](gameId)

  describe("connectPlayer") {
    it("should load a stored Egyptian Ratscrew game as an active game") {
      val gameId             = newGameId
      val (output, isActive) = withProtocol(initGame(gameId)) { env =>
        for
          out    <- env.protocol.connectPlayer(gameId, ers, TestData.existingUser)
          active <- env.protocol.activeGames
        yield out -> active.get(gameId, ers).isDefined
      }
      output shouldBe OutputMessage.PlayerConnectionSuccess(gameId, ers, p1, TestData.existingUser.name)
      isActive shouldBe true
    }
  }

  describe("currentState") {
    it("should return the Egyptian Ratscrew state filtered for the player") {
      val gameId = newGameId
      val output = withProtocol(initGame(gameId))(_.protocol.currentState(gameId, ers, p1))
      output match
        case OutputMessage.GameState(id, recipient, state) =>
          id shouldBe gameId
          recipient shouldBe p1
          state.hcursor.get[String]("code") shouldBe Right("Init")
          state.hcursor.get[String]("player_id") shouldBe Right(p1.show)
          state.hcursor.downField("context").succeeded shouldBe false
        case other => fail(s"Unexpected output $other")
    }
  }

  describe("submitInput") {
    it("should apply the input and send the new state to every player") {
      val gameId          = newGameId
      val (output, state) = withProtocol(initGame(gameId)) { env =>
        for
          out   <- env.protocol.submitInput(gameId, ers, p1, Ready(p1))
          game  <- env.protocol.activeGames.map(_.get(gameId, ers).get)
          state <- game.currentState
        yield out -> state
      }
      output.collect { case OutputMessage.GameState(`gameId`, recipient, _) => recipient } should contain theSameElementsAs List(p1, p2)
      state shouldBe ERsGameState.Init(initCtx, ready = Set(p1))
    }

    it("should return a game error when the game rejects the input") {
      val gameId = newGameId
      val output = withProtocol(ersGame(gameId, ERsGameState.PlayerTurn(initCtx, p2)))(_.protocol.submitInput(gameId, ers, p1, PlayCard(p1)))
      output match
        case List(OutputMessage.GameError(`gameId`, `p1`, code, msg)) =>
          code shouldBe "input_submission_failure"
        case other => fail(s"Unexpected output $other")
    }

    it("should reject an input submitted on behalf of another player") {
      val gameId = newGameId
      withProtocol(initGame(gameId))(_.protocol.submitInput(gameId, ers, p1, Ready(p2))) shouldBe List(
        OutputMessage.ProtocolError(gameId, p1, "illegal_game_input", ProtocolError.IllegalGameInput(p1, p2, gameId, ers).getMessage)
      )
    }

    it("should end the game, persist it and update the players stats when the game is finished") {
      val almostFinished                = ersAlmostFinishedState(p1, p2)
      val gameId                        = newGameId
      val (output, stored, active, upd) = withProtocol(ersGame(gameId, almostFinished)) { env =>
        for
          out    <- env.protocol.submitInput(gameId, ers, p1, Ready(p1))
          stored <- storedState(env, gameId)
          active <- env.protocol.activeGames
          upd    <- env.stats.updates.get
        yield (out, stored, active, upd)
      }
      output.last shouldBe OutputMessage.GameEnded(gameId)
      stored.flatMap(_.data.finishedAt) shouldBe defined
      stored.map(_.state) shouldBe Some(ERsGameState.Finish(almostFinished.context, p1))
      active.activeGames shouldBe empty
      upd shouldBe List(StatUpdate(ers, winners = List(p1), draws = Nil, losers = List(p2)))
    }

    it("should end the game without updating the stats when a player exits") {
      val gameId                = newGameId
      val (output, stored, upd) = withProtocol(initGame(gameId)) { env =>
        for
          out    <- env.protocol.submitInput(gameId, ers, p1, ERsInput.MetaInput.End(p1))
          stored <- storedState(env, gameId)
          upd    <- env.stats.updates.get
        yield (out, stored, upd)
      }
      output.last shouldBe OutputMessage.GameEnded(gameId)
      stored.map(_.state) shouldBe Some(ERsGameState.Exit(initCtx))
      stored.flatMap(_.data.finishedAt) shouldBe defined
      upd shouldBe empty
    }
  }

  describe("saveGame") {
    it("should persist the current state of the active Egyptian Ratscrew game") {
      val gameId = newGameId
      val stored = withProtocol(initGame(gameId)) { env =>
        env.protocol.submitInput(gameId, ers, p1, Ready(p1)) >> env.protocol.saveGame(gameId, ers) >> storedState(env, gameId)
      }
      stored.map(_.state) shouldBe Some(ERsGameState.Init(initCtx, ready = Set(p1)))
    }
  }

  describe("InputParser") {
    val gameId = newGameId
    val player = ConnectedPlayer(gameId, ers, p1, "test")

    def parse(state: ERsGameState, input: String): List[OutputMessage] =
      withProtocol(ersGame(gameId, state)) { env =>
        Ref.of[IO, Option[ConnectedPlayer]](Some(player)).flatMap(InputParser.make[IO](env.protocol).parse(_, input))
      }

    def command(input: ERsInput): String = (GameCommand(gameId, input.asJson): AuthenticatedCommand).asJson.noSpaces

    it("should decode and submit an Egyptian Ratscrew game input") {
      val output = parse(ERsGameState.Init(initCtx), command(Ready(p1)))
      output.collect { case OutputMessage.GameState(`gameId`, recipient, _) => recipient } should contain theSameElementsAs List(p1, p2)
    }

    it("should decode and submit a Discard input") {
      val penalized = ERsGameState.SlapPenalty(initCtx, p1, cats.data.NonEmptyList.one(initCtx.playerHand(p2).head), ERsGameState.PlayerTurn(initCtx, p2))
      val output    = parse(penalized, command(Discard(p1, initCtx.playerHand(p1).head.id)))
      output.collect { case OutputMessage.GameState(`gameId`, recipient, _) => recipient } should contain theSameElementsAs List(p1, p2)
    }

    it("should reject an input of another game type") {
      val warInput = io.circe.Json.obj("input_type" -> "Battle".asJson, "player_id" -> p1.show.asJson)
      val output   = parse(ERsGameState.Init(initCtx), (GameCommand(gameId, warInput): AuthenticatedCommand).asJson.noSpaces)
      output match
        case List(OutputMessage.ProtocolError(`gameId`, `p1`, "invalid_input", _)) => succeed
        case other                                                                 => fail(s"Unexpected output $other")
    }
  }
