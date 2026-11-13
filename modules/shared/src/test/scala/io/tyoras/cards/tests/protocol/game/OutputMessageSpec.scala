package io.tyoras.cards.tests.protocol.game

import io.circe.testing.golden.GoldenCodecTests
import io.tyoras.cards.tests.SharedCodecTestUtils
import org.scalatest.prop.Configuration
import io.tyoras.cards.tests.AllGameOutputMessages
import org.typelevel.discipline.scalatest.FlatSpecDiscipline

import io.circe.Json
import io.circe.syntax.*
import io.tyoras.cards.domain.game.{Game, GameTyp}
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.protocol.game.OutputMessage
import io.tyoras.cards.shared.protocol.game.OutputMessage.{
  AuthError,
  DiscardMessage,
  GameEnded,
  GameError,
  GameState,
  KeepAlive,
  PlayerConnectionSuccess,
  PlayerDisconnected,
  ProtocolError,
  UnsupportedCommand
}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID

class OutputMessageSpec extends AnyFlatSpec with Matchers with EitherValues with SharedCodecTestUtils with FlatSpecDiscipline with Configuration:

  private val gameId = Game.ID(UUID.randomUUID())
  private val userId = User.ID(UUID.randomUUID())

  "Game OutputMessage codec" should "add a message_type discriminator and snake_case fields" in {
    val json = (PlayerConnectionSuccess(gameId, GameTyp.War, userId, "alice"): OutputMessage).asJson.hcursor

    json.get[String]("message_type").value shouldBe "PlayerConnectionSuccess"
    json.get[UUID]("game_id").value shouldBe gameId.unwrap
    json.get[String]("game_type").value shouldBe "war"
    json.get[UUID]("player_id").value shouldBe userId.unwrap
    json.get[String]("player_name").value shouldBe "alice"
  }

  it should "round-trip every message type" in {
    val messages = List[OutputMessage](
      KeepAlive,
      DiscardMessage,
      AuthError("auth", "denied"),
      UnsupportedCommand,
      PlayerConnectionSuccess(gameId, GameTyp.Schnapsen, userId, "alice"),
      PlayerDisconnected(gameId, GameTyp.Schnapsen, userId, "alice"),
      GameState(gameId, userId, Json.obj("turn" -> Json.fromInt(1))),
      GameError(gameId, userId, "invalid_move", "Not your turn"),
      ProtocolError(gameId, userId, "bad_input", "Cannot parse"),
      GameEnded(gameId)
    )

    messages.foreach(m => m.asJson.as[OutputMessage].value shouldBe m)
  }

  // golden JSON codec tests
  checkAll("GoldenCodec[OutputMessage]", GoldenCodecTests[OutputMessage].goldenCodec)
  // every case of the enum
  checkAll("GoldenCodec[AllGameOutputMessages]", GoldenCodecTests[AllGameOutputMessages].goldenCodec)
