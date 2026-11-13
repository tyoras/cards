package io.tyoras.cards.tests.domain.game

import cats.data.NonEmptyList
import cats.syntax.show.*
import io.tyoras.cards.domain.card.Turn
import io.tyoras.cards.domain.game.GameError.{GameAlreadyFinished, GameNotFound, NoPlayersError}
import io.tyoras.cards.domain.game.*
import io.tyoras.cards.domain.game.war.model.PlayerGameState.given
import io.tyoras.cards.domain.game.war.model.{GameContext, GameState, WarInput}
import io.tyoras.cards.tests.{gameIdGen, pick, userIdGen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{ZoneOffset, ZonedDateTime}

class GameEntitySpec extends AnyFlatSpec with Matchers:

  private val now     = ZonedDateTime.of(2026, 1, 1, 12, 0, 0, 0, ZoneOffset.UTC)
  private val user1   = userIdGen.pick
  private val user2   = userIdGen.pick
  private val gameId  = gameIdGen.pick
  private val context = GameContext(Map.empty, now, Turn.firstTurn)
  private val state   = GameState.Exit(context): GameState
  private val other   = GameState.Init(context): GameState

  private val data     = Game.Data[GameState](GameTyp.War, NonEmptyList.of(user1, user2), state, user1, None)
  private val existing = Game.Existing(gameId, now, now, data)

  "Game.Data" should "update its state" in {
    data.withUpdatedState(other).state shouldBe other
  }

  it should "update its end date" in {
    data.withFinishedAt(now).finishedAt shouldBe Some(now)
  }

  it should "be displayed" in {
    val text = data.show
    text should include(user1.toString)
    text should include("not finished")
    data.withFinishedAt(now).show should include(s"finished_at = $now")
  }

  "Game.Existing" should "delegate to its data" in {
    existing.gameType shouldBe GameTyp.War
    existing.players shouldBe data.players
    existing.state shouldBe state
  }

  it should "update its state" in {
    existing.withUpdatedState(other).data shouldBe data.withUpdatedState(other)
  }

  it should "update its end date" in {
    existing.withFinishedAt(now).data.finishedAt shouldBe Some(now)
  }

  it should "be displayed" in {
    val text = existing.show
    text should include(s"id = $gameId")
    text should include("not finished")
  }

  "Game.ID" should "be displayed like its value" in {
    gameId.show shouldBe gameId.toString
  }

  "GameError" should "expose its code and message" in {
    NoPlayersError.code shouldBe "no_players"
    GameNotFound(gameId).code shouldBe "game_not_found"
    GameNotFound(gameId).getMessage should include(gameId.toString)
    GameAlreadyFinished(gameId).code shouldBe "game_already_finished"
    GameAlreadyFinished(gameId).getMessage should include(gameId.toString)
  }

  "GameStateFilter.apply" should "summon the filter of the state" in {
    GameStateFilter[GameState].codec should not be null
  }

  "GameTyp" should "provide the codecs and the filter of its state and input" in {
    val game  = GameTyp.War
    val codec = game.given_Codec_State
    codec.decodeJson(codec(state)) shouldBe Right(state)
    game.given_Decoder_Input should not be null
    game.given_Codec_PlayerState should not be null
    game.given_GameStateFilter_State.codec should not be null
  }

  it should "provide the input decoder" in {
    val decoded = GameTyp.War.given_Decoder_Input.decodeJson(io.circe.parser.parse(s"""{"input_type":"Ready","player_id":"$user1"}""").toOption.get)
    decoded shouldBe Right(WarInput.GameInput.Ready(user1))
  }
