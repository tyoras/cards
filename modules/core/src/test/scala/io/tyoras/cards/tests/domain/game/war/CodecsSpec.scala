package io.tyoras.cards.tests.domain.game.war

import cats.data.*
import cats.effect.SyncIO
import io.chrisdavenport.fuuid.FUUID
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.circe.syntax.*
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.domain.game.war.model.*
import io.circe.parser.decode
import io.tyoras.cards.domain.card.{Card, Hand}
import io.tyoras.cards.domain.card.Rank.{Ace, Eight, Jack, Ten}
import io.tyoras.cards.domain.card.Suit.{Club, Heart, Spade}
import io.tyoras.cards.domain.game.war.model.GameState.WarTurn.BattleRound

import java.time.ZonedDateTime

class CodecsSpec extends AnyFlatSpec with Matchers:

  val playerId1: PlayerId = FUUID.randomFUUID[SyncIO].unsafeRunSync()
  val playerId2: PlayerId = FUUID.randomFUUID[SyncIO].unsafeRunSync()
  val playerId3: PlayerId = FUUID.randomFUUID[SyncIO].unsafeRunSync()

  val player1FirstCard: Card = Card(Spade, Ten())

  val player1: Player             = Player(playerId1, List(player1FirstCard, Card(Heart, Jack())))
  val player2: Player             = Player(playerId2, List(Card(Club, Ace())))
  val playerWithEmptyHand: Player = Player(playerId3, Hand.empty)

  val context = GameContext(Map(playerId1 -> player1), ZonedDateTime.now(), Turn.firstTurn)

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
    val context = GameContext(Map(playerId1 -> player1, playerId2 -> player2), ZonedDateTime.now(), Turn.firstTurn, List(Elimination(playerId1, Turn(1))))
    val json    = context.asJson
    val decoded = decode[GameContext](json.noSpaces)
    decoded shouldBe Right(context)
  }

  it should "round-trip correctly with empty eliminations" in {
    val context = GameContext(Map(playerId1 -> player1), ZonedDateTime.now(), Turn.firstTurn, List.empty)
    val json    = context.asJson
    val decoded = decode[GameContext](json.noSpaces)
    decoded shouldBe Right(context)
  }

  "BattleRound encoder and decoder" should "round-trip correctly with all cards played" in {
    val round = GameState.WarTurn.BattleRound(
      NonEmptySet.of(playerId1, playerId2),
      Map(playerId1 -> player1FirstCard, playerId2 -> player2.hand.head),
      Map(playerId1 -> player1.hand(1), playerId2  -> Card(Heart, Eight()))
    )
    val json    = round.asJson
    val decoded = decode[BattleRound](json.noSpaces)
    decoded shouldBe Right(round)
  }

  it should "round-trip correctly with no cards played" in {
    val round   = GameState.WarTurn.BattleRound(NonEmptySet.of(playerId1), Map.empty, Map.empty)
    val json    = round.asJson
    val decoded = decode[BattleRound](json.noSpaces)
    decoded shouldBe Right(round)
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

  "GameState.BattleTurn encoder and decoder" should "round-trip correctly with no played cards" in {
    val battle: GameState = GameState.BattleTurn(context)
    val json              = battle.asJson
    json.hcursor.get[String]("code") shouldBe Right("BattleTurn")
    val decoded = decode[GameState](json.noSpaces)
    decoded shouldBe Right(battle)
  }

  it should "round-trip correctly with played cards" in {
    val battle: GameState = GameState.BattleTurn(context, Map(playerId1 -> player1FirstCard))
    val json              = battle.asJson
    json.hcursor.get[String]("code") shouldBe Right("BattleTurn")
    val decoded = decode[GameState](json.noSpaces)
    decoded shouldBe Right(battle)
  }

  "GameState.WarTurn encoder and decoder" should "round-trip correctly" in {
    val firstWarRound =
      GameState.WarTurn.BattleRound(NonEmptySet.of(playerId1, playerId2), Map(playerId1 -> player1FirstCard, playerId2 -> player2.hand.head), Map.empty)
    val war: GameState = GameState.WarTurn(context, NonEmptyList.of(firstWarRound))
    val json           = war.asJson
    json.hcursor.get[String]("code") shouldBe Right("WarTurn")
    val decoded = decode[GameState](json.noSpaces)
    decoded shouldBe Right(war)
  }

  "GameState.PlayerWinTurn encoder and decoder" should "round-trip correctly with no acknowledgments" in {
    val win: GameState = GameState.PlayerWinTurn(context, playerId1, Set(player1FirstCard), Set.empty)
    val json           = win.asJson
    json.hcursor.get[String]("code") shouldBe Right("PlayerWinTurn")
    val decoded = decode[GameState](json.noSpaces)
    decoded shouldBe Right(win)
  }

  it should "round-trip correctly with acknowledgments" in {
    val win: GameState = GameState.PlayerWinTurn(context, playerId1, Set(player1FirstCard), Set.empty, Set(playerId1))
    val json           = win.asJson
    json.hcursor.get[String]("code") shouldBe Right("PlayerWinTurn")
    val decoded = decode[GameState](json.noSpaces)
    decoded shouldBe Right(win)
  }

  "WarInput.GameInput.Ready encoder and decoder" should "round-trip correctly" in {
    val ready: WarInput = WarInput.GameInput.Ready(playerId1)
    val json            = ready.asJson
    json.hcursor.get[String]("input_type") shouldBe Right("Ready")
    val decoded = decode[WarInput](json.noSpaces)
    decoded shouldBe Right(ready)
  }

  "WarInput.GameInput.PlayCard encoder and decoder" should "round-trip correctly" in {
    val playCard: WarInput = WarInput.GameInput.PlayCard(playerId1, player1FirstCard.id)
    val json               = playCard.asJson
    json.hcursor.get[String]("input_type") shouldBe Right("PlayCard")
    val decoded = decode[WarInput](json.noSpaces)
    decoded shouldBe Right(playCard)
  }

  "WarInput.MetaInput.Restart encoder and decoder" should "round-trip correctly" in {
    val restart: WarInput = WarInput.MetaInput.Restart(playerId1)
    val json              = restart.asJson
    json.hcursor.get[String]("input_type") shouldBe Right("Restart")
    val decoded = decode[WarInput](json.noSpaces)
    decoded shouldBe Right(restart)
  }

  "WarInput.MetaInput.End encoder and decoder" should "round-trip correctly" in {
    val end: WarInput = WarInput.MetaInput.End(playerId1)
    val json          = end.asJson
    json.hcursor.get[String]("input_type") shouldBe Right("End")
    val decoded = decode[WarInput](json.noSpaces)
    decoded shouldBe Right(end)
  }
