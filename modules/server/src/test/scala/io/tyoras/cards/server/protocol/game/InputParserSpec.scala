package io.tyoras.cards.server.protocol.game

import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import io.circe.Json
import io.circe.syntax.*
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.domain.game.war.model.{WarGameState, WarInput}
import io.tyoras.cards.domain.game.war.model.WarInput.GameInput.Ready
import io.tyoras.cards.domain.game.{Game, GameTyp}
import io.tyoras.cards.server.endpoints.utils.{TestAuthService, TestData}
import io.tyoras.cards.server.protocol.fixtures.*
import io.tyoras.cards.shared.protocol.game.Commands.AuthCommand
import io.tyoras.cards.shared.protocol.game.Commands.AuthenticatedCommand
import io.tyoras.cards.shared.protocol.game.Commands.AuthenticatedCommand.*
import io.tyoras.cards.shared.protocol.game.OutputMessage
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

class InputParserSpec extends AnyFunSpec with Matchers:
  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val p1     = TestData.existingUser.id
  private val p2     = newUserId
  private val gameId = newGameId
  private val player = ConnectedPlayer(gameId, GameTyp.War, p1, "test")

  /** Parses the input with the given connected player and returns the output and the connected player after parsing */
  private def parse(initialPlayer: Option[ConnectedPlayer], input: String): (List[OutputMessage], Option[ConnectedPlayer]) =
    (for
      games  <- InMemoryGameService.make(warGame(gameId, WarGameState.Init(twoPlayersContext(p1, p2))))
      stats  <- RecordingGameStatService.make
      result <- GameProtocol.make[IO](new TestAuthService, games, stats).use { protocol =>
        val parser = InputParser.make[IO](protocol)
        for
          ref    <- Ref.of[IO, Option[ConnectedPlayer]](initialPlayer)
          output <- parser.parse(ref, input)
          after  <- ref.get
        yield output -> after
      }
    yield result).unsafeRunSync()

  private def authCommand(jwt: String, id: Game.ID = gameId): String = AuthCommand(id, GameTyp.War, jwt).asJson.noSpaces

  private def command(cmd: AuthenticatedCommand): String = cmd.asJson.noSpaces

  describe("any player") {
    it("should discard blank inputs") {
      parse(None, "  ") shouldBe (List(OutputMessage.DiscardMessage)       -> None)
      parse(Some(player), "") shouldBe (List(OutputMessage.DiscardMessage) -> Some(player))
    }
  }

  describe("unauthenticated player") {
    it("should be connected with a valid auth command") {
      val (output, connected) = parse(None, authCommand(TestData.existingUserAuthToken.value))
      output shouldBe List(OutputMessage.PlayerConnectionSuccess(gameId, GameTyp.War, p1, TestData.existingUser.name))
      connected shouldBe Some(player)
    }
    it("should not be connected with an invalid token") {
      parse(None, authCommand("invalid")) shouldBe (List(OutputMessage.AuthError("token_auth", "Invalid token: unknown token")) -> None)
    }
    it("should not be connected to an unknown game") {
      val unknown          = newGameId
      val (output, player) = parse(None, authCommand(TestData.existingUserAuthToken.value, unknown))
      output shouldBe List(OutputMessage.AuthError("game_not_found", ProtocolError.ActiveGameNotFound(unknown, GameTyp.War).getMessage))
      player shouldBe None
    }
    it("should reject anything else than an auth command") {
      parse(None, "not json") shouldBe (List(OutputMessage.UnsupportedCommand)                    -> None)
      parse(None, command(StateCommand(gameId))) shouldBe (List(OutputMessage.UnsupportedCommand) -> None)
    }
  }

  describe("authenticated player") {
    it("should get the current game state") {
      val (output, _) = parse(Some(player), command(StateCommand(gameId)))
      output.collect { case OutputMessage.GameState(`gameId`, recipient, _) => recipient } shouldBe List(p1)
    }
    it("should submit a game input") {
      val input       = (Ready(p1): WarInput).asJson
      val (output, _) = parse(Some(player), command(GameCommand(gameId, input)))
      output.collect { case OutputMessage.GameState(`gameId`, recipient, _) => recipient } should contain theSameElementsAs List(p1, p2)
    }
    it("should reject an invalid game input") {
      val (output, _) = parse(Some(player), command(GameCommand(gameId, Json.obj("unknown" -> Json.Null))))
      output match
        case List(OutputMessage.ProtocolError(`gameId`, `p1`, "invalid_input", _)) => succeed
        case other                                                                 => fail(s"Unexpected output $other")
    }
    it("should reject an invalid command") {
      val (output, connected) = parse(Some(player), "not json")
      output.map(_.productPrefix) shouldBe List("ProtocolError")
      connected shouldBe Some(player)
    }
    it("should be disconnected with the quit command") {
      parse(Some(player), command(QuitCommand(gameId))) shouldBe (List(OutputMessage.PlayerDisconnected(gameId, GameTyp.War, p1, "test")) -> None)
    }
  }
