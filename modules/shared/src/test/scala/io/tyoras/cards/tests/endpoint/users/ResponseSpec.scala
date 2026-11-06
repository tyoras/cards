package io.tyoras.cards.tests.endpoint.users

import io.circe.syntax.*
import io.github.iltotore.iron.*
import io.scalaland.chimney.dsl.*
import io.tyoras.cards.domain.game.GameTyp
import io.tyoras.cards.domain.game.stats.model.PlayerGameStat
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.endpoint.users.Payloads.Response
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime
import java.util.UUID

class ResponseSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val createdAt = ZonedDateTime.parse("2026-01-01T10:00:00Z")
  private val updatedAt = ZonedDateTime.parse("2026-01-02T10:00:00Z")
  private val userId    = User.ID(UUID.randomUUID())

  "Response.User codec" should "round-trip through JSON" in {
    val user = Response.User(userId, createdAt, updatedAt, "Jane", "About Jane")

    user.asJson.as[Response.User].value shouldBe user
  }

  it should "encode the id as a flat UUID" in {
    Response.User(userId, createdAt, updatedAt, "Jane", "About Jane").asJson.hcursor.get[UUID]("id").value shouldBe userId.unwrap
  }

  "PartialTransformer from Response.User to Existing" should "build a domain user from valid data" in {
    val result = Response.User(userId, createdAt, updatedAt, "Jane", "About Jane").transformIntoPartial[User.Existing]

    result.asOption shouldBe Some(User.Existing(userId, createdAt, updatedAt, User.Data(User.Name("Jane"), User.About("About Jane"))))
  }

  it should "fail when the name is invalid" in {
    Response.User(userId, createdAt, updatedAt, "  ", "About Jane").transformIntoPartial[User.Existing].asOption shouldBe None
  }

  it should "fail when the about is invalid" in {
    Response.User(userId, createdAt, updatedAt, "Jane", "").transformIntoPartial[User.Existing].asOption shouldBe None
  }

  "UserGameStats transformer" should "transform an existing player game stat" in {
    val stat = PlayerGameStat.Existing(
      PlayerGameStat.ID(UUID.randomUUID()),
      createdAt,
      updatedAt,
      PlayerGameStat.Data(userId, GameTyp.War, 3, 1, 2)
    )

    stat.transformInto[Response.UserGameStats] shouldBe Response.UserGameStats(GameTyp.War, 3, 1, 2, updatedAt)
  }

  "UserGameStats codec" should "encode with snake_case field names" in {
    val json = Response.UserGameStats(GameTyp.Schnapsen, 5, 0, 4, updatedAt).asJson.hcursor

    json.get[String]("game").value shouldBe "schnapsen"
    json.get[Int]("won").value shouldBe 5
    json.get[Int]("draw").value shouldBe 0
    json.get[Int]("lost").value shouldBe 4
    json.get[ZonedDateTime]("updated_at").value shouldBe updatedAt
  }

  it should "round-trip through JSON" in {
    val stats = Response.UserGameStats(GameTyp.War, 1, 2, 3, updatedAt)

    stats.asJson.as[Response.UserGameStats].value shouldBe stats
  }
