package io.tyoras.cards.tests.domain.game.ers

import cats.data.NonEmptyList
import io.circe.parser.decode
import io.circe.syntax.*
import io.tyoras.cards.domain.card.Rank.{Ace, Jack, Ten}
import io.tyoras.cards.domain.card.Suit.{Club, Heart, Spade}
import io.tyoras.cards.domain.card.{Card, Hand, Turn}
import io.tyoras.cards.domain.game.ers.codecs.given
import io.tyoras.cards.domain.game.ers.model.*
import io.tyoras.cards.tests.{pick, userIdGen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime

class CodecsSpec extends AnyFlatSpec with Matchers:

  val playerId1: PlayerId = userIdGen.pick
  val playerId2: PlayerId = userIdGen.pick
  val playerId3: PlayerId = userIdGen.pick

  val player1FirstCard: Card = Card(Spade, Ten())

  val player1: Player             = Player(playerId1, List(player1FirstCard, Card(Heart, Jack())))
  val player2: Player             = Player(playerId2, List(Card(Club, Ace())))
  val playerWithEmptyHand: Player = Player(playerId3, Hand.empty)

  val context = GameContext(Map(playerId1 -> player1), ZonedDateTime.now(), Turn.firstTurn, NonEmptyList.one(playerId1))

  "Player encoder and decoder" should "round-trip correctly with non-empty hand" in {
    val json    = player1.asJson
    val decoded = decode[Player](json.noSpaces)
    decoded shouldBe Right(player1)
  }

  it should "round-trip correctly with empty hand" in {
    val player  = Player(playerId1, List.empty)
    val json    = player.asJson
    val decoded = decode[Player](json.noSpaces)
    decoded shouldBe Right(player)
  }

  "Elimination encoder and decoder" should "round-trip correctly" in {
    val elimination = Elimination(playerId1, Turn(1))
    val json        = elimination.asJson
    val decoded     = decode[Elimination](json.noSpaces)
    decoded shouldBe Right(elimination)
  }

  "GameContext encoder and decoder" should "round-trip correctly with players and eliminations" in {
    val context = GameContext(
      Map(playerId1 -> player1, playerId2 -> player2),
      ZonedDateTime.now(),
      Turn.firstTurn,
      NonEmptyList.of(playerId1, playerId2),
      List(Elimination(playerId1, Turn(1)))
    )
    val json    = context.asJson
    val decoded = decode[GameContext](json.noSpaces)
    decoded shouldBe Right(context)
  }

  it should "round-trip correctly with empty eliminations" in {
    val context = GameContext(Map(playerId1 -> player1), ZonedDateTime.now(), Turn.firstTurn, NonEmptyList.one(playerId1), List.empty)
    val json    = context.asJson
    val decoded = decode[GameContext](json.noSpaces)
    decoded shouldBe Right(context)
  }

  "GameState.Init encoder and decoder" should "round-trip correctly with no ready players" in {
    val init: GameState = GameState.Init(context)
    val json            = init.asJson
    json.hcursor.get[String]("code") shouldBe Right("Init")
    val decoded = decode[GameState](json.noSpaces)
    decoded shouldBe Right(init)
  }

  it should "round-trip correctly with ready players" in {
    val init: GameState = GameState.Init(context, Set(playerId1))
    val json            = init.asJson
    json.hcursor.get[String]("code") shouldBe Right("Init")
    val decoded = decode[GameState](json.noSpaces)
    decoded shouldBe Right(init)
  }

  "GameState.PlayerWinTurn encoder and decoder" should "round-trip correctly with no acknowledgments" in {
    val win: GameState = GameState.PlayerWinTurn(context, playerId1, NonEmptyList.one(player1FirstCard), Set.empty)
    val json           = win.asJson
    json.hcursor.get[String]("code") shouldBe Right("PlayerWinTurn")
    val decoded = decode[GameState](json.noSpaces)
    decoded shouldBe Right(win)
  }

  it should "round-trip correctly with acknowledgments" in {
    val win: GameState = GameState.PlayerWinTurn(context, playerId1, NonEmptyList.one(player1FirstCard), Set.empty, Set(playerId1))
    val json           = win.asJson
    json.hcursor.get[String]("code") shouldBe Right("PlayerWinTurn")
    val decoded = decode[GameState](json.noSpaces)
    decoded shouldBe Right(win)
  }

  "GameState.SlapPenalty encoder and decoder" should "round-trip correctly with a ChallengeTurn original state" in {
    val original           = GameState.ChallengeTurn(context, playerId1, playerId2, 3, 2, NonEmptyList.one(player1FirstCard))
    val penalty: GameState =
      GameState.SlapPenalty(context, playerId1, NonEmptyList.one(player1FirstCard), original, List(Card(Club, Ace())), Set(playerId2))
    val json = penalty.asJson
    json.hcursor.get[String]("code") shouldBe Right("SlapPenalty")
    json.hcursor.downField("original_state").get[String]("code") shouldBe Right("ChallengeTurn")
    val decoded = decode[GameState](json.noSpaces)
    decoded shouldBe Right(penalty)
  }

  it should "round-trip correctly with a PlayerTurn original state" in {
    val original           = GameState.PlayerTurn(context, playerId1, List(player1FirstCard))
    val penalty: GameState = GameState.SlapPenalty(context, playerId2, NonEmptyList.one(player1FirstCard), original)
    val json               = penalty.asJson
    json.hcursor.downField("original_state").get[String]("code") shouldBe Right("PlayerTurn")
    val decoded = decode[GameState](json.noSpaces)
    decoded shouldBe Right(penalty)
  }

  it should "reject an original state that is neither PlayerTurn nor ChallengeTurn" in {
    val original           = GameState.PlayerTurn(context, playerId1)
    val penalty: GameState = GameState.SlapPenalty(context, playerId2, NonEmptyList.one(player1FirstCard), original)
    val json               = penalty.asJson.mapObject(_.add("original_state", (GameState.Init(context): GameState).asJson))
    decode[GameState](json.noSpaces).isLeft shouldBe true
  }

  "GameState.PlayerTurn encoder and decoder" should "round-trip correctly with an empty pile" in {
    val turn: GameState = GameState.PlayerTurn(context, playerId1)
    val json            = turn.asJson
    json.hcursor.get[String]("code") shouldBe Right("PlayerTurn")
    decode[GameState](json.noSpaces) shouldBe Right(turn)
  }

  it should "round-trip correctly with a pile" in {
    val turn: GameState = GameState.PlayerTurn(context, playerId1, List(player1FirstCard, Card(Club, Ace())))
    decode[GameState](turn.asJson.noSpaces) shouldBe Right(turn)
  }

  "GameState.ChallengeTurn encoder and decoder" should "round-trip correctly" in {
    val challenge: GameState = GameState.ChallengeTurn(context, playerId1, playerId2, 4, 1, NonEmptyList.of(player1FirstCard, Card(Club, Ace())))
    val json                 = challenge.asJson
    json.hcursor.get[String]("code") shouldBe Right("ChallengeTurn")
    json.hcursor.get[Int]("remaining_tries") shouldBe Right(1)
    decode[GameState](json.noSpaces) shouldBe Right(challenge)
  }

  "GameState.Finish encoder and decoder" should "round-trip correctly" in {
    val finish: GameState = GameState.Finish(context, playerId1)
    val json              = finish.asJson
    json.hcursor.get[String]("code") shouldBe Right("Finish")
    decode[GameState](json.noSpaces) shouldBe Right(finish)
  }

  "GameState.Exit encoder and decoder" should "round-trip correctly" in {
    val exit: GameState = GameState.Exit(context)
    val json            = exit.asJson
    json.hcursor.get[String]("code") shouldBe Right("Exit")
    decode[GameState](json.noSpaces) shouldBe Right(exit)
  }

  "PlayerTurn | ChallengeTurn encoder and decoder" should "round-trip a PlayerTurn" in {
    val turn: GameState.PlayerTurn | GameState.ChallengeTurn = GameState.PlayerTurn(context, playerId1, List(player1FirstCard))
    decode[GameState.PlayerTurn | GameState.ChallengeTurn](turn.asJson.noSpaces) shouldBe Right(turn)
  }

  it should "round-trip a ChallengeTurn" in {
    val turn: GameState.PlayerTurn | GameState.ChallengeTurn = GameState.ChallengeTurn(context, playerId1, playerId2, 3, 3, NonEmptyList.one(player1FirstCard))
    decode[GameState.PlayerTurn | GameState.ChallengeTurn](turn.asJson.noSpaces) shouldBe Right(turn)
  }

  it should "reject any other game state" in {
    val result = decode[GameState.PlayerTurn | GameState.ChallengeTurn]((GameState.Exit(context): GameState).asJson.noSpaces)
    result.left.map(_.getMessage) shouldBe Left("DecodingFailure at : Expected player-turn or challenge-turn, got exit")
  }

  it should "reject an invalid json" in {
    decode[GameState.PlayerTurn | GameState.ChallengeTurn]("""{"code":"Unknown"}""").isLeft shouldBe true
  }

  "PlayerId key encoder and decoder" should "round-trip a map keyed by player id" in {
    val map  = Map(playerId1 -> 1, playerId2 -> 2)
    val json = map.asJson
    json.hcursor.get[Int](playerId1.toString) shouldBe Right(1)
    decode[Map[PlayerId, Int]](json.noSpaces) shouldBe Right(map)
  }

  it should "reject a key that is not a UUID" in {
    decode[Map[PlayerId, Int]]("""{"not-a-uuid":1}""").isLeft shouldBe true
  }

  "ERSInput.GameInput.Ready encoder and decoder" should "round-trip correctly" in {
    val ready: ERSInput = ERSInput.GameInput.Ready(playerId1)
    val json            = ready.asJson
    json.hcursor.get[String]("input_type") shouldBe Right("Ready")
    val decoded = decode[ERSInput](json.noSpaces)
    decoded shouldBe Right(ready)
  }

  "ERSInput.GameInput.PlayCard encoder and decoder" should "round-trip correctly" in {
    val playCard: ERSInput = ERSInput.GameInput.PlayCard(playerId1, player1FirstCard.id)
    val json               = playCard.asJson
    json.hcursor.get[String]("input_type") shouldBe Right("PlayCard")
    val decoded = decode[ERSInput](json.noSpaces)
    decoded shouldBe Right(playCard)
  }

  "ERSInput.GameInput.Slap encoder and decoder" should "round-trip correctly" in {
    val slap: ERSInput = ERSInput.GameInput.Slap(playerId1)
    val json           = slap.asJson
    json.hcursor.get[String]("input_type") shouldBe Right("Slap")
    val decoded = decode[ERSInput](json.noSpaces)
    decoded shouldBe Right(slap)
  }

  "ERSInput.MetaInput.Restart encoder and decoder" should "round-trip correctly" in {
    val restart: ERSInput = ERSInput.MetaInput.Restart(playerId1)
    val json              = restart.asJson
    json.hcursor.get[String]("input_type") shouldBe Right("Restart")
    val decoded = decode[ERSInput](json.noSpaces)
    decoded shouldBe Right(restart)
  }

  "ERSInput.MetaInput.End encoder and decoder" should "round-trip correctly" in {
    val end: ERSInput = ERSInput.MetaInput.End(playerId1)
    val json          = end.asJson
    json.hcursor.get[String]("input_type") shouldBe Right("End")
    val decoded = decode[ERSInput](json.noSpaces)
    decoded shouldBe Right(end)
  }
