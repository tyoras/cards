package io.tyoras.cards.game.schnapsen

import io.tyoras.cards.game.schnapsen.model.{GameContext, InvalidPlayer}
import io.tyoras.cards.game.schnapsen.tests._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ModelSpec extends AnyFlatSpec with Matchers with EitherValues with ScalaCheckDrivenPropertyChecks {

  "Player marriage points" should "be 0 when he has not won any cards" in {
    forAll(playerGen -> "player") { player =>
      whenever(player.wonCards.isEmpty) {
        player.potentialMarriagePoints shouldBe 0
      }
    }
  }

  it should "be 0 when he has won cards but not completed any marriage" in {
    forAll(playerGen -> "player") { p =>
      whenever(p.wonCards.nonEmpty) {
        val player = p.copy(marriages = Nil)
        player.potentialMarriagePoints shouldBe 0
      }
    }
  }

  it should "be the marriage score sum when he has won cards and completed marriage" in {
    forAll(playerGen -> "player") { player =>
      whenever(player.wonCards.nonEmpty && player.marriages.nonEmpty) {
        val expected = player.marriages.map(_.status.score).sum
        player.potentialMarriagePoints shouldBe expected
      }
    }
  }

  "Player" should "have failed marriage when he has not won any cards and completed some marriage" in {
    forAll(playerGen -> "player") { player =>
      whenever(player.marriages.nonEmpty && player.wonCards.isEmpty) {
        player.hasFailedMarriage shouldBe true
      }
    }
  }

  it should "not have failed marriage when he has won cards" in {
    forAll(playerGen -> "player") { player =>
      whenever(player.wonCards.nonEmpty) {
        player.hasFailedMarriage shouldBe false
      }
    }
  }

  it should "not have failed marriage when he has not completed any marriage" in {
    forAll(playerGen -> "player") { p =>
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

  "GameContext.player" should "return an error when the id does not belong to one of the players" in {
    forAll(gameContextGen -> "gameContext", playerIdGen -> "id") { (gameContext, id) =>
      whenever(gameContext.player1.id != id && gameContext.player2.id != id) {
        gameContext.player(id).left.value shouldBe InvalidPlayer(s"Unknown player id ($id)")
      }
    }
  }

  it should "return player1 when the id match its one" in {
    forAll(gameContextGen -> "gameContext") { gameContext =>
        gameContext.player(gameContext.player1.id).right.value shouldBe gameContext.player1
    }
  }

  it should "return player2 when the id match its one" in {
    forAll(gameContextGen -> "gameContext") { gameContext =>
      gameContext.player(gameContext.player2.id).right.value shouldBe gameContext.player2
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
