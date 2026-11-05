package io.tyoras.cards.tests.domain.game

import cats.data.NonEmptyList
import io.tyoras.cards.domain.game.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.tests.{gameIdGen, pick, userIdGen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime
import io.github.iltotore.iron.*

class GameModelSpec extends AnyFlatSpec with Matchers:

  val now: ZonedDateTime             = ZonedDateTime.now()
  val userId1: User.ID               = userIdGen.pick
  val userId2: User.ID               = userIdGen.pick
  val gameId: Game.ID                = gameIdGen.pick
  val players: NonEmptyList[User.ID] = NonEmptyList.of(userId1, userId2)

  "Game.Count refined type" should "allow zero" in {
    val count: Game.Count = 0
    count should be(0)
  }

  it should "allow positive integers" in {
    val count: Game.Count = 100
    count should be(100)
  }

  "Game.Percentage refined type" should "allow minimum value (0.0)" in {
    val percent: Game.Percentage = 0.0
    percent should be(0.0)
  }

  it should "allow maximum value (100.0)" in {
    val percent: Game.Percentage = 100.0
    percent should be(100.0)
  }
