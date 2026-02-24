package io.tyoras.cards.tests.domain.game.war.model

import cats.effect.SyncIO
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.card.Rank.{Ace, Jack, Ten}
import io.tyoras.cards.domain.card.Suit.{Club, Heart, Spade}
import io.tyoras.cards.domain.card.{Card, Hand}
import io.tyoras.cards.domain.game.war.model.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime

class GameContextSpec extends AnyFlatSpec with Matchers {

  val playerId1: PlayerId = FUUID.randomFUUID[SyncIO].unsafeRunSync()
  val playerId2: PlayerId = FUUID.randomFUUID[SyncIO].unsafeRunSync()
  val playerId3: PlayerId = FUUID.randomFUUID[SyncIO].unsafeRunSync()

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
    val context = GameContext(Map(playerId1 -> player1), startedAt, 5)
    val updated = context.eliminatePlayer(playerId1)
    updated.eliminations should contain(Elimination(playerId1, 5))
  }

  it should "preserve existing eliminations" in {
    val context = GameContext(Map(playerId1 -> player1, playerId2 -> player2), startedAt, 3, List(Elimination(playerId2, 2)))
    val updated = context.eliminatePlayer(playerId1)
    updated.eliminations should have size 2
    updated.eliminations should contain(Elimination(playerId1, 3))
    updated.eliminations should contain(Elimination(playerId2, 2))
  }

  "GameContext.incrementTurnNumber" should "increment turn number by one" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, 5)
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
      5,
      List(Elimination(playerId2, 3), Elimination(playerId3, 4))
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
      3,
      List(Elimination(playerId3, 2))
    )
    context.allEliminated shouldBe false
  }

}
