package io.tyoras.cards.tests.domain.game.stats.model

import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.tyoras.cards.domain.game.stats.model.{PlayerGameStat, PlayerGameStats}
import io.tyoras.cards.domain.game.GameTyp
import io.chrisdavenport.fuuid.FUUID
import io.github.iltotore.iron.*

import java.time.ZonedDateTime
import java.util.UUID

class PlayerGameStatsSpec extends AnyFlatSpec with Matchers {

  private val playerId     = FUUID.fromUUID(UUID.randomUUID())
  private val stat1Id      = FUUID.fromUUID(UUID.randomUUID())
  private val stat2Id      = FUUID.fromUUID(UUID.randomUUID())
  private val creationDate = ZonedDateTime.now()
  private val updateDate   = ZonedDateTime.now()

  "PlayerGameStats" should "create with playerId and multiple stats" in {
    val stat1       = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen, 5, 0, 3))
    val stat2       = PlayerGameStat.Existing(stat2Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.War, 10, 2, 5))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.of(stat1, stat2))
    playerStats.stats shouldBe NonEmptyList.of(stat1, stat2)
  }

  it should "calculate totalPlayed correctly with single stat" in {
    val stat        = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen, 5, 2, 3))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.one(stat))
    playerStats.totalPlayed shouldBe 10
  }

  it should "calculate totalPlayed correctly with multiple stats" in {
    val stat1       = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen, 5, 0, 3))
    val stat2       = PlayerGameStat.Existing(stat2Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.War, 10, 2, 5))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.of(stat1, stat2))
    playerStats.totalPlayed shouldBe 25
  }

  it should "calculate totalPlayed as 0 with empty stats" in {
    val stat1       = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen))
    val stat2       = PlayerGameStat.Existing(stat2Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.War))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.of(stat1, stat2))
    playerStats.totalPlayed shouldBe 0
  }

  it should "calculate totalWon correctly with single stat" in {
    val stat        = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen, 5, 2, 3))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.one(stat))
    playerStats.totalWon shouldBe 5
  }

  it should "calculate totalWon correctly with multiple stats" in {
    val stat1       = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen, 5, 0, 3))
    val stat2       = PlayerGameStat.Existing(stat2Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.War, 10, 2, 5))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.of(stat1, stat2))
    playerStats.totalWon shouldBe 15
  }

  it should "calculate totalWon as 0 with empty stats" in {
    val stat1       = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen))
    val stat2       = PlayerGameStat.Existing(stat2Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.War))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.of(stat1, stat2))
    playerStats.totalWon shouldBe 0
  }

  it should "calculate totalLost correctly with single stat" in {
    val stat        = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen, 5, 2, 3))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.one(stat))
    playerStats.totalLost shouldBe 3
  }

  it should "calculate totalLost correctly with multiple stats" in {
    val stat1       = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen, 5, 0, 3))
    val stat2       = PlayerGameStat.Existing(stat2Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.War, 10, 2, 5))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.of(stat1, stat2))
    playerStats.totalLost shouldBe 8
  }

  it should "calculate totalLost as 0 with empty stats" in {
    val stat1       = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen))
    val stat2       = PlayerGameStat.Existing(stat2Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.War))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.of(stat1, stat2))
    playerStats.totalLost shouldBe 0
  }

  it should "calculate totalWinRate correctly" in {
    val stat1       = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen, 5, 0, 5))
    val stat2       = PlayerGameStat.Existing(stat2Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.War, 5, 0, 5))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.of(stat1, stat2))
    playerStats.totalWinRate shouldBe 50.0
  }

  it should "return 0 totalWinRate with empty stats" in {
    val stat1       = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen))
    val stat2       = PlayerGameStat.Existing(stat2Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.War))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.of(stat1, stat2))
    playerStats.totalWinRate shouldBe 0.0
  }

  it should "calculate totalLoseRate correctly" in {
    val stat1       = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen, 5, 0, 5))
    val stat2       = PlayerGameStat.Existing(stat2Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.War, 5, 0, 5))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.of(stat1, stat2))
    playerStats.totalLoseRate shouldBe 50.0
  }

  it should "return 0 totalLoseRate with empty stats" in {
    val stat1       = PlayerGameStat.Existing(stat1Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.Schnapsen))
    val stat2       = PlayerGameStat.Existing(stat2Id, creationDate, updateDate, PlayerGameStat.Data(playerId, GameTyp.War))
    val playerStats = PlayerGameStats(playerId, NonEmptyList.of(stat1, stat2))
    playerStats.totalLoseRate shouldBe 0.0
  }
}
