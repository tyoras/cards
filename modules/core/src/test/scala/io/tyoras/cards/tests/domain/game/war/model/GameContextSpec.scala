package io.tyoras.cards.tests.domain.game.war.model

import io.tyoras.cards.domain.card.Rank.{Ace, Jack, Ten}
import io.tyoras.cards.domain.card.Suit.{Club, Heart, Spade}
import io.tyoras.cards.domain.card.{Card, Hand, Turn}
import io.tyoras.cards.domain.game.war.model.*
import io.tyoras.cards.tests.{pick, userIdGen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime

class GameContextSpec extends AnyFlatSpec with Matchers {

  val playerId1: PlayerId = userIdGen.pick
  val playerId2: PlayerId = userIdGen.pick
  val playerId3: PlayerId = userIdGen.pick

  val player1FirstCard: Card = Card(Spade, Ten())

  val player1: Player             = Player(playerId1, List(player1FirstCard, Card(Heart, Jack())))
  val player2: Player             = Player(playerId2, List(Card(Club, Ace())))
  val playerWithEmptyHand: Player = Player(playerId3, Hand.empty)

  val startedAt: ZonedDateTime = ZonedDateTime.now()

  "Player.eliminated" should "return true when hand is empty" in {
    playerWithEmptyHand.eliminated shouldBe true
  }

  it should "return false when hand is not empty" in {
    player1.eliminated shouldBe false
    player2.eliminated shouldBe false
  }

  "GameContext.player" should "return Some(Player) when player exists" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn)
    context.player(playerId1) shouldBe Some(player1)
  }

  it should "return None when player does not exist" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn)
    context.player(playerId2) shouldBe None
  }

  "GameContext.pickFirstCard" should "return Some(Card) when player has cards" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn)
    context.pickFirstCard(playerId1) shouldBe Some(player1FirstCard)
  }

  it should "return None when player has no cards" in {
    val context = GameContext(Map(playerId3 -> playerWithEmptyHand), startedAt, Turn.firstTurn)
    context.pickFirstCard(playerId3) shouldBe None
  }

  it should "return None when player does not exist" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn)
    context.pickFirstCard(playerId2) shouldBe None
  }

  "GameContext.eliminatePlayer" should "add elimination record" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn(5))
    val updated = context.eliminatePlayer(playerId1)
    updated.eliminations should contain(Elimination(playerId1, Turn(5)))
  }

  it should "preserve existing eliminations" in {
    val context = GameContext(Map(playerId1 -> player1, playerId2 -> player2), startedAt, Turn(3), List(Elimination(playerId2, Turn(2))))
    val updated = context.eliminatePlayer(playerId1)
    updated.eliminations should have size 2
    updated.eliminations should contain(Elimination(playerId1, Turn(3)))
    updated.eliminations should contain(Elimination(playerId2, Turn(2)))
  }

  "GameContext.incrementTurnNumber" should "increment turn number by one" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn(5))
    context.incrementTurnNumber.turnNumber shouldBe 6
  }

  it should "increment from first turn" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn)
    context.incrementTurnNumber.turnNumber shouldBe 2
  }

  "GameContext.allEliminated" should "return true when all players except one are eliminated" in {
    val context = GameContext(
      Map(playerId1 -> player1, playerId2 -> player2, playerId3 -> playerWithEmptyHand),
      startedAt,
      Turn(5),
      List(Elimination(playerId2, Turn(3)), Elimination(playerId3, Turn(4)))
    )
    context.allEliminated shouldBe true
  }

  it should "return false when no players are eliminated" in {
    val context = GameContext(
      Map(playerId1 -> player1, playerId2 -> player2),
      startedAt,
      Turn.firstTurn
    )
    context.allEliminated shouldBe false
  }

  it should "return false when some but not all except one are eliminated" in {
    val context = GameContext(
      Map(playerId1 -> player1, playerId2 -> player2, playerId3 -> playerWithEmptyHand),
      startedAt,
      Turn(3),
      List(Elimination(playerId3, Turn(2)))
    )
    context.allEliminated shouldBe false
  }

  "Player" should "have an empty hand by default" in {
    Player(playerId1).hand shouldBe empty
  }

  "GameContext.toString" should "display the turn and the players" in {
    val text = GameContext(Map(playerId1 -> player1), startedAt, Turn(5)).toString
    text should include("turn 5")
    text should include(playerId1.toString)
  }

  "GameContext.playerHand" should "return the hand of the player" in {
    GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn).playerHand(playerId1) shouldBe player1.hand
  }

  it should "return an empty hand for an unknown player" in {
    GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn).playerHand(playerId2) shouldBe empty
  }

  "GameContext.playersCardCount" should "count the cards of each player" in {
    val context = GameContext(Map(playerId1 -> player1, playerId3 -> playerWithEmptyHand), startedAt, Turn.firstTurn)
    context.playersCardCount shouldBe Map(playerId1 -> 2, playerId3 -> 0)
  }

}
