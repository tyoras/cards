package io.tyoras.cards.game.schnapsen

import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards._
import io.tyoras.cards.game.schnapsen.model.Marriage.{Common, Royal}
import io.tyoras.cards.game.schnapsen.model._
import io.tyoras.cards.game.schnapsen.tests._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ModelSpec extends AnyFlatSpec with Matchers with EitherValues with ScalaCheckDrivenPropertyChecks {

  "Player.toString" should "work" in {
    val diamondTen = Card(Diamond, Ten())
    val spadeJack = Card(Spade, Jack())
    val player = Player(FUUID.fuuid("f8377258-437b-409c-bd4b-f7637c885539"), "Yoan", List(diamondTen, spadeJack))
    player.toString shouldBe s"name = Yoan (f8377258-437b-409c-bd4b-f7637c885539) \t| score = 0 \t| hand = ${diamondTen.toString} ${spadeJack.toString}"
  }

  "Player marriage points" should "be 0 when he has not won any cards" in {
    forAll(playerGen() -> "player") { player =>
      whenever(player.wonCards.isEmpty) {
        player.potentialMarriagePoints shouldBe 0
      }
    }
  }

  it should "be 0 when he has won cards but not completed any marriage" in {
    forAll(playerGen() -> "player") { p =>
      whenever(p.wonCards.nonEmpty) {
        val player = p.copy(marriages = Nil)
        player.potentialMarriagePoints shouldBe 0
      }
    }
  }

  it should "be the marriage score sum when he has won cards and completed marriage" in {
    forAll(playerGen() -> "player") { player =>
      whenever(player.wonCards.nonEmpty && player.marriages.nonEmpty) {
        val expected = player.marriages.map(_.status.score).sum
        player.potentialMarriagePoints shouldBe expected
      }
    }
  }

  "Player" should "have failed marriage when he has not won any cards and completed some marriage" in {
    forAll(playerGen() -> "player") { player =>
      whenever(player.marriages.nonEmpty && player.wonCards.isEmpty) {
        player.hasFailedMarriage shouldBe true
      }
    }
  }

  it should "not have failed marriage when he has won cards" in {
    forAll(playerGen() -> "player") { player =>
      whenever(player.wonCards.nonEmpty) {
        player.hasFailedMarriage shouldBe false
      }
    }
  }

  it should "not have failed marriage when he has not completed any marriage" in {
    forAll(playerGen() -> "player") { p =>
      val player = p.copy(marriages = Nil)
      player.hasFailedMarriage shouldBe false
    }
  }

  "PlayerInfo reset" should "reset only the player score" in {
    forAll(playerInfoGen -> "playerInfo") { playerInfo =>
      val reset = playerInfo.reset
      reset.id shouldBe playerInfo.id
      reset.name shouldBe playerInfo.name
      reset.score shouldBe 7
    }
  }

  "GameRound.toString" should "work" in {
    forAll(gameRoundGen -> "gameRound") { gameRound =>
    val toString = gameRound.toString
      toString should include ("Dealer")
      toString should include ("Forehand")
      toString should include ("Trump card")
      toString should include ("Talon")
    }
  }

  "GameRound.trumpSuit" should "work" in {
    forAll(gameRoundGen -> "gameRound") { gameRound =>
      gameRound.trumpSuit shouldBe gameRound.trumpCard.suit
    }
  }

  "GameRound.roles" should "map their roles to the players" in {
    forAll(gameRoundGen -> "gameRound") { gameRound =>
      gameRound.roles(Forehand) shouldBe gameRound.forehand
      gameRound.roles(Dealer) shouldBe gameRound.dealer
    }
  }

  "GameRound.playersById" should "map their ids to the players" in {
    forAll(gameRoundGen -> "gameRound") { gameRound =>
      gameRound.playersById(gameRound.forehand.id) shouldBe gameRound.forehand
      gameRound.playersById(gameRound.dealer.id) shouldBe gameRound.dealer
    }
  }

  "GameRound.updatePlayer" should "update the dealer if the updated player matches its id" in {
    forAll(gameRoundGen -> "gameRound") { gameRound =>
      val expectedDealer = gameRound.dealer.copy(name = "test")
      val updatedRound = gameRound.updatePlayer(expectedDealer)
      updatedRound.dealer shouldBe expectedDealer
      updatedRound.forehand shouldBe gameRound.forehand
    }
  }

  it should "update the forehand if the updated player matches its id" in {
    forAll(gameRoundGen -> "gameRound") { gameRound =>
      val expectedForehand= gameRound.forehand.copy(name = "test")
      val updatedRound = gameRound.updatePlayer(expectedForehand)
      updatedRound.dealer shouldBe gameRound.dealer
      updatedRound.forehand shouldBe expectedForehand
    }
  }

  "Marriage suit constructor" should "have both the king and the queen of the suit for common marriage" in {
    forAll(suitGen -> "suit") { suit =>
      val commonMarriage = Marriage(suit)
      commonMarriage.king.rank shouldBe King(4)
      commonMarriage.king.suit shouldBe suit
      commonMarriage.queen.rank shouldBe Queen(3)
      commonMarriage.queen.suit shouldBe suit
      commonMarriage.status shouldBe Common
    }
  }

  it should "have both the king and the queen of the suit for royal marriage" in {
    forAll(suitGen -> "suit") { suit =>
      val royalMarriage = Marriage(suit, Royal)
      royalMarriage.king.rank shouldBe King(4)
      royalMarriage.king.suit shouldBe suit
      royalMarriage.queen.rank shouldBe Queen(3)
      royalMarriage.queen.suit shouldBe suit
      royalMarriage.status shouldBe Royal
    }
  }

  "Marriage.toString" should "work for common marriage" in {
    val commonMarriage = Marriage(Diamond)
    commonMarriage.toString shouldBe s"Common marriage between ${Card(Diamond, King(4))} and ${Card(Diamond, Queen(3))}"
  }

  it should "work for royal marriage" in {
    val commonMarriage = Marriage(Diamond, Royal)
    commonMarriage.toString shouldBe s"Royal marriage between ${Card(Diamond, King(4))} and ${Card(Diamond, Queen(3))}"
  }

  "GameContext.player" should "return an error when the id does not belong to one of the players" in {
    forAll(gameContextGen -> "gameContext", playerIdGen -> "id") { (gameContext, id) =>
      whenever(gameContext.player1.id != id && gameContext.player2.id != id) {
        gameContext.player(id).left.value shouldBe InvalidPlayer(s"Unknown player id ($id)")
      }
    }
  }

  it should "return player1 when the id match its one" in {
    forAll(gameContextGen -> "gameContext") { gameContext =>
        gameContext.player(gameContext.player1.id).getOrElse(fail("Either was left")) shouldBe gameContext.player1
    }
  }

  it should "return player2 when the id match its one" in {
    forAll(gameContextGen -> "gameContext") { gameContext =>
      gameContext.player(gameContext.player2.id).getOrElse(fail("Either was left")) shouldBe gameContext.player2
    }
  }

  "GameContext.reset" should "return a reset game context" in {
    forAll(gameContextGen -> "gameContext", zonedDateTimeGen -> "date") { (gameContext, date) =>
      val resetGC = GameContext.reset(gameContext, date)
      resetGC.startedAt shouldBe date
      resetGC.player1.score shouldBe 7
      resetGC.player2.score shouldBe 7
    }
  }
}
