package io.tyoras.cards.tests.domain.game.stats.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.tyoras.cards.domain.game.stats.model.PlayerGameStat
import io.tyoras.cards.domain.game.*
import io.chrisdavenport.fuuid.FUUID
import io.github.iltotore.iron.*

import java.time.ZonedDateTime
import java.util.UUID

class PlayerGameStateSpec extends AnyFlatSpec with Matchers {

  private val playerId = FUUID.fromUUID(UUID.randomUUID())
  private val gameType = GameTyp.Schnapsen
  private val now      = ZonedDateTime.now()

  "PlayerGameStat.Data" should "create with default values" in {
    val stat = PlayerGameStat.Data(playerId, gameType)
    stat.playerId shouldBe playerId
    stat.game shouldBe gameType
    stat.won shouldBe 0
    stat.draw shouldBe 0
    stat.lost shouldBe 0
  }

  it should "create with explicit values" in {
    val stat = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    stat.won shouldBe 5
    stat.draw shouldBe 2
    stat.lost shouldBe 3
  }

  it should "calculate played count correctly" in {
    val stat = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    stat.played shouldBe 10
  }

  it should "calculate winRate correctly" in {
    val stat = PlayerGameStat.Data(playerId, gameType, 5, 0, 5)
    stat.winRate shouldBe 50.0
  }

  it should "return 0 winRate when no games played" in {
    val stat = PlayerGameStat.Data(playerId, gameType, 0, 0, 0)
    stat.winRate shouldBe 0.0
  }

  it should "calculate loseRate correctly" in {
    val stat = PlayerGameStat.Data(playerId, gameType, 5, 0, 5)
    stat.loseRate shouldBe 50.0
  }

  it should "return 0 loseRate when no games played" in {
    val stat = PlayerGameStat.Data(playerId, gameType, 0, 0, 0)
    stat.loseRate shouldBe 0.0
  }

  it should "increment won count" in {
    val stat    = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    val updated = stat.incrementWon
    updated.won shouldBe 6
    updated.draw shouldBe 2
    updated.lost shouldBe 3
  }

  it should "increment draw count" in {
    val stat    = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    val updated = stat.incrementDraw
    updated.won shouldBe 5
    updated.draw shouldBe 3
    updated.lost shouldBe 3
  }

  it should "increment lost count" in {
    val stat    = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    val updated = stat.incrementLost
    updated.won shouldBe 5
    updated.draw shouldBe 2
    updated.lost shouldBe 4
  }

  "PlayerGameStat.Existing" should "delegate playerId to data" in {
    val data     = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    val id       = FUUID.fromUUID(UUID.randomUUID())
    val existing = PlayerGameStat.Existing(id, now, now, data)
    existing.playerId shouldBe playerId
  }

  it should "delegate game to data" in {
    val data     = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    val id       = FUUID.fromUUID(UUID.randomUUID())
    val existing = PlayerGameStat.Existing(id, now, now, data)
    existing.game shouldBe gameType
  }

  it should "delegate won, draw, lost to data" in {
    val data     = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    val id       = FUUID.fromUUID(UUID.randomUUID())
    val existing = PlayerGameStat.Existing(id, now, now, data)
    existing.won shouldBe 5
    existing.draw shouldBe 2
    existing.lost shouldBe 3
  }

  it should "increment won and update timestamp" in {
    val data     = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    val id       = FUUID.fromUUID(UUID.randomUUID())
    val existing = PlayerGameStat.Existing(id, now, now, data)
    val updated  = existing.incrementWon
    updated.won shouldBe 6
    updated.updatedAt.isAfter(existing.updatedAt) shouldBe true
  }

  it should "increment draw and update timestamp" in {
    val data     = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    val id       = FUUID.fromUUID(UUID.randomUUID())
    val existing = PlayerGameStat.Existing(id, now, now, data)
    val updated  = existing.incrementDraw
    updated.draw shouldBe 3
    updated.updatedAt.isAfter(existing.updatedAt) shouldBe true
  }

  it should "increment lost and update timestamp" in {
    val data     = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    val id       = FUUID.fromUUID(UUID.randomUUID())
    val existing = PlayerGameStat.Existing(id, now, now, data)
    val updated  = existing.incrementLost
    updated.lost shouldBe 4
    updated.updatedAt.isAfter(existing.updatedAt) shouldBe true
  }

  it should "preserve id and createdAt on increment" in {
    val data     = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    val id       = FUUID.fromUUID(UUID.randomUUID())
    val existing = PlayerGameStat.Existing(id, now, now, data)
    val updated  = existing.incrementWon
    updated.id shouldBe id
    updated.createdAt shouldBe now
  }
}
