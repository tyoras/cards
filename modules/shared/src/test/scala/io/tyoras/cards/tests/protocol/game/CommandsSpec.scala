package io.tyoras.cards.tests.protocol.game

import io.circe.testing.golden.GoldenCodecTests
import io.tyoras.cards.tests.SharedCodecTestUtils
import org.scalatest.prop.Configuration
import io.tyoras.cards.tests.AllAuthenticatedCommands
import org.typelevel.discipline.scalatest.FlatSpecDiscipline

import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import io.tyoras.cards.domain.game.{Game, GameTyp}
import io.tyoras.cards.shared.protocol.game.Commands.{AuthCommand, AuthenticatedCommand}
import io.tyoras.cards.shared.protocol.game.Commands.AuthenticatedCommand.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID

class CommandsSpec extends AnyFlatSpec with Matchers with EitherValues with SharedCodecTestUtils with FlatSpecDiscipline with Configuration:

  private val gameId = Game.ID(UUID.randomUUID())

  "AuthCommand codec" should "encode with snake_case field names" in {
    val json = AuthCommand(gameId, GameTyp.War, "a.b.c").asJson.hcursor

    json.get[UUID]("game_id").value shouldBe gameId.unwrap
    json.get[String]("game_type").value shouldBe "war"
    json.get[String]("jwt").value shouldBe "a.b.c"
  }

  it should "decode a JSON payload" in {
    val decoded = parse(s"""{"game_id":"${gameId.unwrap}","game_type":"schnapsen","jwt":"tok"}""").flatMap(_.as[AuthCommand])

    decoded.value shouldBe AuthCommand(gameId, GameTyp.Schnapsen, "tok")
  }

  "AuthenticatedCommand" should "expose the game id of every command" in {
    List[AuthenticatedCommand](QuitCommand(gameId), StateCommand(gameId), GameCommand(gameId, Json.Null)).map(_.gameId).distinct shouldBe List(gameId)
  }

  "AuthenticatedCommand codec" should "add a command_type discriminator" in {
    (QuitCommand(gameId): AuthenticatedCommand).asJson.hcursor.get[String]("command_type").value shouldBe "QuitCommand"
  }

  it should "round-trip every command" in {
    val commands = List[AuthenticatedCommand](
      QuitCommand(gameId),
      StateCommand(gameId),
      GameCommand(gameId, Json.obj("action" -> Json.fromString("play")))
    )

    commands.foreach(c => c.asJson.as[AuthenticatedCommand].value shouldBe c)
  }

  it should "decode a game command with snake_case fields" in {
    val decoded = parse(s"""{"command_type":"GameCommand","game_id":"${gameId.unwrap}","input":{"x":1}}""").flatMap(_.as[AuthenticatedCommand])

    decoded.value shouldBe GameCommand(gameId, Json.obj("x" -> Json.fromInt(1)))
  }

  it should "reject an unknown command type" in {
    parse(s"""{"command_type":"Nope","game_id":"${gameId.unwrap}"}""").flatMap(_.as[AuthenticatedCommand]).isLeft shouldBe true
  }

  // golden JSON codec tests
  checkAll("GoldenCodec[AuthCommand]", GoldenCodecTests[AuthCommand].goldenCodec)
  checkAll("GoldenCodec[AuthenticatedCommand]", GoldenCodecTests[AuthenticatedCommand].goldenCodec)
  // every case of the enum
  checkAll("GoldenCodec[AllAuthenticatedCommands]", GoldenCodecTests[AllAuthenticatedCommands].goldenCodec)
