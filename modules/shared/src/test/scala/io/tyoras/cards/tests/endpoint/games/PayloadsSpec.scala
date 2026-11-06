package io.tyoras.cards.tests.endpoint.games

import io.circe.testing.golden.GoldenCodecTests
import io.tyoras.cards.tests.SharedCodecTestUtils
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FlatSpecDiscipline

import cats.data.NonEmptyList
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import io.scalaland.chimney.dsl.*
import io.tyoras.cards.domain.game.{Game, GameTyp}
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.endpoint.games.Payloads.{Request, Response}
import io.tyoras.cards.shared.endpoint.games.Payloads.Response.Game.given
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime
import java.util.UUID

class PayloadsSpec extends AnyFlatSpec with Matchers with EitherValues with SharedCodecTestUtils with FlatSpecDiscipline with Configuration:

  private val createdAt = ZonedDateTime.parse("2026-01-01T10:00:00Z")
  private val updatedAt = ZonedDateTime.parse("2026-01-02T10:00:00Z")
  private val gameId    = Game.ID(UUID.randomUUID())
  private val player1   = User.ID(UUID.randomUUID())
  private val player2   = User.ID(UUID.randomUUID())

  private val existingGame: Game.Existing[Json] =
    Game.Existing(
      gameId,
      createdAt,
      updatedAt,
      Game.Data(GameTyp.War.asInstanceOf[GameTyp[Json, ?]], NonEmptyList.of(player1, player2), Json.Null, player1, None)
    )

  private val expectedResponse = Response.Game(gameId, createdAt, updatedAt, GameTyp.War, NonEmptyList.of(player1, player2))

  "Creation request" should "round-trip through JSON" in {
    val creation = Request.Creation(List(UUID.randomUUID(), UUID.randomUUID()))

    creation.asJson.as[Request.Creation].value shouldBe creation
  }

  it should "decode a list of player UUIDs" in {
    val id      = UUID.randomUUID()
    val decoded = parse(s"""{"players":["$id"]}""").flatMap(_.as[Request.Creation])

    decoded.value shouldBe Request.Creation(List(id))
  }

  "Response.Game.fromExistingGame" should "copy every exposed field from an existing game" in {
    Response.Game.fromExistingGame(existingGame) shouldBe expectedResponse
  }

  "Response.Game transformer" should "transform an existing game into a response" in {
    (existingGame: Game.Existing[?]).transformInto[Response.Game] shouldBe expectedResponse
  }

  "Response.Game codec" should "encode with snake_case field names and flat ids" in {
    val json = expectedResponse.asJson.hcursor

    json.get[UUID]("id").value shouldBe gameId.unwrap
    json.get[String]("game_type").value shouldBe "war"
    json.get[ZonedDateTime]("created_at").value shouldBe createdAt
    json.get[ZonedDateTime]("updated_at").value shouldBe updatedAt
    json.get[List[UUID]]("players").value shouldBe List(player1.unwrap, player2.unwrap)
  }

  it should "round-trip through JSON" in {
    expectedResponse.asJson.as[Response.Game].value shouldBe expectedResponse
  }

  it should "fail to decode an empty player list" in {
    val json = expectedResponse.asJson.mapObject(_.add("players", Json.arr()))

    json.as[Response.Game].isLeft shouldBe true
  }

  // golden JSON codec tests
  checkAll("GoldenCodec[Creation]", GoldenCodecTests[Request.Creation].goldenCodec)
  checkAll("GoldenCodec[Game]", GoldenCodecTests[Response.Game].goldenCodec)
