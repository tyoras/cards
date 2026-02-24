package io.tyoras.cards.cli.game.war

import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.cli.game.war.InputParser
import io.tyoras.cards.cli.game.war.WarCliError.{InvalidInput, InvalidState}
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.{Card, Hand, Rank}
import io.tyoras.cards.domain.game.war.model.*
import io.tyoras.cards.domain.game.war.model.GameInput.Ready
import io.tyoras.cards.domain.game.war.model.GameState.{Exit, Finish, Init, PlayerWinTurn, WarTurn}
import io.tyoras.cards.domain.game.war.model.MetaInput.Restart
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.time.ZonedDateTime

class InputParserSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  implicit val loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]

  val playerId1: PlayerId = FUUID.randomFUUID[IO].unsafeRunSync()
  val playerId2: PlayerId = FUUID.randomFUUID[IO].unsafeRunSync()

  val card1: Card = Card(Spade, Ace())

  val player1: Player = Player(playerId1, List(card1, Card(Heart, King())))
  val player2: Player = Player(playerId2, List(Card(Diamond, Queen())))

  val context: GameContext = GameContext(Map(playerId1 -> player1, playerId2 -> player2), ZonedDateTime.now(), Turn.firstTurn)

  val parser: InputParser[IO] = InputParser[IO]

  "InputParser.parse" should "return End input for \\q command" in {
    val initState = Init(context)
    parser.parse(initState, playerId1, "\\q").asserting { inputs =>
      inputs.head shouldBe MetaInput.End(playerId1)
      inputs.size shouldBe 1
    }
  }

  it should "return Restart input for \\r command" in {
    val initState = Init(context)
    parser.parse(initState, playerId1, "\\r").asserting { inputs =>
      inputs.head shouldBe Restart(playerId1)
      inputs.size shouldBe 1
    }
  }

  it should "return Ready inputs for all not ready players in Init state" in {
    val initState = Init(context)
    parser.parse(initState, playerId1, "").asserting { inputs =>
      inputs.toList should contain theSameElementsAs List(Ready(playerId1), Ready(playerId2))
    }
  }

  it should "return Ready inputs for remaining not ready players in Init state" in {
    val initState = Init(context, Set(playerId1))
    parser.parse(initState, playerId1, "").asserting { inputs =>
      inputs.head shouldBe Ready(playerId2)
      inputs.size shouldBe 1
    }
  }

  it should "fail with InvalidState when all players are ready in Init state" in {
    val initState = Init(context, Set(playerId1, playerId2))
    parser.parse(initState, playerId1, "").attempt.asserting { result =>
      result.isLeft shouldBe true
      result.left.exists(_ == InvalidState) shouldBe true
    }
  }

  it should "return PlayCard input with first card in BattleTurn state" in {
    val battleTurn = GameState.BattleTurn(context)
    parser.parse(battleTurn, playerId1, "").asserting { inputs =>
      inputs.head shouldBe GameInput.PlayCard(playerId1, card1)
      inputs.size shouldBe 1
    }
  }

  it should "fail with InvalidState when player has no cards in BattleTurn state" in {
    val emptyHandPlayer      = Player(playerId1, Hand.empty)
    val contextWithEmptyHand = context.copy(players = Map(playerId1 -> emptyHandPlayer, playerId2 -> player2))
    val battleTurn           = GameState.BattleTurn(contextWithEmptyHand)
    parser.parse(battleTurn, playerId1, "").attempt.asserting { result =>
      result.isLeft shouldBe true
      result.left.exists(_ == InvalidState) shouldBe true
    }
  }

  it should "return PlayCard input with first card in WarTurn state" in {
    val round   = WarTurn.BattleRound(NonEmptySet.of(playerId1, playerId2), Map.empty, Map.empty)
    val warTurn = WarTurn(context, NonEmptyList.one(round))
    parser.parse(warTurn, playerId1, "").asserting { inputs =>
      inputs.head shouldBe GameInput.PlayCard(playerId1, card1)
      inputs.size shouldBe 1
    }
  }

  it should "fail with InvalidState when player has no cards in WarTurn state" in {
    val emptyHandPlayer      = Player(playerId1, Nil)
    val contextWithEmptyHand = context.copy(players = Map(playerId1 -> emptyHandPlayer, playerId2 -> player2))
    val round                = WarTurn.BattleRound(NonEmptySet.of(playerId1, playerId2), Map.empty, Map.empty)
    val warTurn              = WarTurn(contextWithEmptyHand, NonEmptyList.one(round))
    parser.parse(warTurn, playerId1, "").attempt.asserting { result =>
      result.isLeft shouldBe true
      result.left.exists(_ == InvalidState) shouldBe true
    }
  }

  it should "return Ready inputs for all not acked players in PlayerWinTurn state" in {
    val winTurn = PlayerWinTurn(context, playerId1, Set(card1), Set.empty)
    parser.parse(winTurn, playerId1, "").asserting { inputs =>
      inputs.toList should contain theSameElementsAs List(Ready(playerId1), Ready(playerId2))
    }
  }

  it should "return Ready inputs for remaining not acked players in PlayerWinTurn state" in {
    val winTurn = PlayerWinTurn(context, playerId1, Set(card1), Set.empty, Set(playerId1))
    parser.parse(winTurn, playerId1, "").asserting { inputs =>
      inputs.head shouldBe Ready(playerId2)
      inputs.size shouldBe 1
    }
  }

  it should "fail with InvalidState when all players acked in PlayerWinTurn state" in {
    val winTurn = PlayerWinTurn(context, playerId1, Set(card1), Set.empty, Set(playerId1, playerId2))
    parser.parse(winTurn, playerId1, "").attempt.asserting { result =>
      result.isLeft shouldBe true
      result.left.exists(_ == InvalidState) shouldBe true
    }
  }

  it should "fail with InvalidInput in Finish state" in {
    val finish = Finish(context, playerId1)
    parser.parse(finish, playerId1, "any input").attempt.asserting { result =>
      result.isLeft shouldBe true
      result.left.exists(_ == InvalidInput) shouldBe true
    }
  }

  it should "fail with InvalidState in Exit state" in {
    val exit = Exit(context)
    parser.parse(exit, playerId1, "any input").attempt.asserting { result =>
      result.isLeft shouldBe true
      result.left.exists(_ == InvalidState) shouldBe true
    }
  }

  it should "prioritize quit command over state-specific parsing" in {
    val battleTurn = GameState.BattleTurn(context)
    parser.parse(battleTurn, playerId1, "\\q").asserting { inputs =>
      inputs.head shouldBe MetaInput.End(playerId1)
      inputs.size shouldBe 1
    }
  }

  it should "prioritize restart command over state-specific parsing" in {
    val battleTurn = GameState.BattleTurn(context)
    parser.parse(battleTurn, playerId1, "\\r").asserting { inputs =>
      inputs.head shouldBe Restart(playerId1)
      inputs.size shouldBe 1
    }
  }
}
