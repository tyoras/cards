package io.tyoras.cards.tests.domain.game.ers.model

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.tyoras.cards.domain.card.Rank.{Ace, Jack, Ten}
import io.tyoras.cards.domain.card.Suit.{Club, Heart, Spade}
import io.tyoras.cards.domain.card.{Card, Hand, Turn}
import io.tyoras.cards.domain.game.ers.model.*
import io.tyoras.cards.domain.user.model.User
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime
import java.util.UUID

class GameContextSpec extends AnyFlatSpec with Matchers {

  val playerId1: PlayerId = User.ID(UUID.randomUUID)
  val playerId2: PlayerId = User.ID(UUID.randomUUID)
  val playerId3: PlayerId = User.ID(UUID.randomUUID)

  val player1FirstCard: Card = Card(Spade, Ten())

  val player1: Player             = Player(playerId1, List(player1FirstCard, Card(Heart, Jack())))
  val player2: Player             = Player(playerId2, List(Card(Club, Ace())))
  val playerWithEmptyHand: Player = Player(playerId3, Hand.empty)

  val startedAt: ZonedDateTime = ZonedDateTime.now()

  "Player.hasCards" should "return false when hand is empty" in {
    playerWithEmptyHand.hasCards shouldBe false
  }

  it should "return true when hand is not empty" in {
    player1.hasCards shouldBe true
    player2.hasCards shouldBe true
  }

  "GameContext.player" should "return Some(Player) when player exists" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn, NonEmptyList.one(playerId1))
    context.player(playerId1) shouldBe Some(player1)
  }

  it should "return None when player does not exist" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn, NonEmptyList.one(playerId1))
    context.player(playerId2) shouldBe None
  }

  "GameContext.pickFirstCard" should "return Some(Card) when player has cards" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn, NonEmptyList.one(playerId1))
    context.pickFirstCard(playerId1) shouldBe Some(player1FirstCard)
  }

  it should "return None when player has no cards" in {
    val context = GameContext(Map(playerId3 -> playerWithEmptyHand), startedAt, Turn.firstTurn, NonEmptyList.one(playerId3))
    context.pickFirstCard(playerId3) shouldBe None
  }

  it should "return None when player does not exist" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn, NonEmptyList.one(playerId1))
    context.pickFirstCard(playerId2) shouldBe None
  }

  "GameContext.eliminatePlayer" should "add elimination record" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn(5), NonEmptyList.one(playerId1))
    val updated = context.eliminatePlayer(playerId1)
    updated.eliminations should contain(Elimination(playerId1, Turn(5)))
  }

  it should "preserve existing eliminations" in {
    val context = GameContext(
      Map(playerId1 -> player1, playerId2 -> player2),
      startedAt,
      Turn(3),
      NonEmptyList.of(playerId1, playerId2),
      List(Elimination(playerId2, Turn(2)))
    )
    val updated = context.eliminatePlayer(playerId1)
    updated.eliminations should have size 2
    updated.eliminations should contain(Elimination(playerId1, Turn(3)))
    updated.eliminations should contain(Elimination(playerId2, Turn(2)))
  }

  "GameContext.incrementTurnNumber" should "increment turn number by one" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn(5), NonEmptyList.one(playerId1))
    context.incrementTurnNumber.turnNumber shouldBe 6
  }

  it should "increment from first turn" in {
    val context = GameContext(Map(playerId1 -> player1), startedAt, Turn.firstTurn, NonEmptyList.one(playerId1))
    context.incrementTurnNumber.turnNumber shouldBe 2
  }

  "GameContext.allEliminated" should "return true when all players except one are eliminated" in {
    val context = GameContext(
      Map(playerId1 -> player1, playerId2 -> player2, playerId3 -> playerWithEmptyHand),
      startedAt,
      Turn(5),
      NonEmptyList.of(playerId1, playerId2, playerId3),
      List(Elimination(playerId2, Turn(3)), Elimination(playerId3, Turn(4)))
    )
    context.allEliminated shouldBe true
  }

  it should "return false when no players are eliminated" in {
    val context = GameContext(
      Map(playerId1 -> player1, playerId2 -> player2),
      startedAt,
      Turn.firstTurn,
      NonEmptyList.of(playerId1, playerId2)
    )
    context.allEliminated shouldBe false
  }

  it should "return false when some but not all except one are eliminated" in {
    val context = GameContext(
      Map(playerId1 -> player1, playerId2 -> player2, playerId3 -> playerWithEmptyHand),
      startedAt,
      Turn(3),
      NonEmptyList.of(playerId1, playerId2, playerId3),
      List(Elimination(playerId3, Turn(2)))
    )
    context.allEliminated shouldBe false
  }

  private def ctx(players: Player*): GameContext =
    GameContext(players.map(p => p.id -> p).toMap, startedAt, Turn.firstTurn, NonEmptyList.fromListUnsafe(players.map(_.id).toList))

  "Player Show" should "display the id and the hand" in {
    player1.show shouldBe s"$playerId1 [$player1FirstCard|${Card(Heart, Jack())}]"
    playerWithEmptyHand.show shouldBe s"$playerId3 []"
  }

  "GameContext.toString" should "describe the game" in {
    val description = ctx(player1).toString
    description should startWith(s"Egyptian Ratscrew game started at $startedAt | turn 1 | players: [")
    description should include(playerId1.toString)
  }

  "GameContext.nextPlayer" should "return the other player in a 2 players game" in {
    val context = ctx(player1, player2)
    context.nextPlayerWithCards(playerId1) shouldBe Some(playerId2)
    context.nextPlayerWithCards(playerId2) shouldBe Some(playerId1)
  }

  it should "skip players without cards" in {
    ctx(player1, playerWithEmptyHand, player2).nextPlayerWithCards(playerId1) shouldBe Some(playerId2)
  }

  it should "return None when no other player has cards" in {
    ctx(player1, playerWithEmptyHand).nextPlayerWithCards(playerId1) shouldBe None
  }

  it should "return the following player with cards when the current player has no cards" in {
    ctx(player1, playerWithEmptyHand, player2).nextPlayerWithCards(playerId3) shouldBe Some(playerId2)
  }

  it should "wrap around the player order" in {
    ctx(player1, player2, playerWithEmptyHand).nextPlayerWithCards(playerId2) shouldBe Some(playerId1)
  }

  it should "return the first player with cards for an unknown player" in {
    ctx(playerWithEmptyHand, player1, player2).nextPlayerWithCards(User.ID(UUID.randomUUID)) shouldBe Some(playerId1)
  }

  it should "follow the player order in a game with more than 2 players" in {
    val p3      = Player(playerId3, List(Card(Club, Ten())))
    val context = ctx(player1, player2, p3)
    context.nextPlayerWithCards(playerId1) shouldBe Some(playerId2)
    context.nextPlayerWithCards(playerId2) shouldBe Some(playerId3)
    context.nextPlayerWithCards(playerId3) shouldBe Some(playerId1)
  }

  "GameContext.updatePlayer" should "update an existing player" in {
    val updated = ctx(player1, player2).updatePlayer(playerId1)(_.copy(hand = Nil))
    updated.player(playerId1) shouldBe Some(Player(playerId1, Nil))
    updated.player(playerId2) shouldBe Some(player2)
  }

  it should "do nothing when the player does not exist" in {
    val context = ctx(player1)
    context.updatePlayer(playerId2)(_.copy(hand = Nil)) shouldBe context
  }

  "GameContext.isEliminated" should "tell whether a player has been eliminated" in {
    val context = ctx(player1, playerWithEmptyHand).eliminatePlayer(playerId3)
    context.isEliminated(playerId3) shouldBe true
    context.isEliminated(playerId1) shouldBe false
  }

  "GameContext.playerHand" should "return the player hand" in {
    ctx(player1).playerHand(playerId1) shouldBe player1.hand
  }

  it should "return an empty hand for an unknown player" in {
    ctx(player1).playerHand(playerId2) shouldBe Nil
  }

  "GameContext.playersCardCount" should "count the cards of every player" in {
    ctx(player1, player2, playerWithEmptyHand).playersCardCount shouldBe Map(
      playerId1 -> Card.Count(2),
      playerId2 -> Card.Count(1),
      playerId3 -> Card.Count(0)
    )
  }

}
