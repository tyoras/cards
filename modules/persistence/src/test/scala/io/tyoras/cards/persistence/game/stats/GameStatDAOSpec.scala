package io.tyoras.cards.persistence.game.stats

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.tyoras.cards.persistence.game.stats.GameStatDAO
import io.tyoras.cards.domain.game.stats.model.PlayerGameStat
import io.tyoras.cards.domain.game.GameTyp
import io.chrisdavenport.fuuid.FUUID

import java.time.ZonedDateTime
import java.util.UUID
import io.github.iltotore.iron.*
import org.scalatest.EitherValues

class GameStatDAOSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val playerId = FUUID.fromUUID(UUID.randomUUID())
  private val id       = FUUID.fromUUID(UUID.randomUUID())
  private val gameType = GameTyp.Schnapsen
  private val now      = ZonedDateTime.now()

  "GameStatsDAO.Data" should "convert from and to domain Data" in {
    val domainData = PlayerGameStat.Data(playerId, gameType, 5, 2, 3)
    val daoData    = GameStatDAO.Data.fromDomain(domainData)

    daoData.playerId shouldBe domainData.playerId
    daoData.game shouldBe domainData.game
    daoData.won shouldBe domainData.won
    daoData.draw shouldBe domainData.draw
    daoData.lost shouldBe domainData.lost

    daoData.toDomain.value shouldBe domainData
  }

  "GameStatsDAO.Existing" should "convert from and to domain Existing" in {
    val domainData     = PlayerGameStat.Data(playerId, gameType, 1, 0, 0)
    val domainExisting = PlayerGameStat.Existing(id, now, now, domainData)
    val daoExisting    = GameStatDAO.Existing.fromDomain(domainExisting)

    daoExisting.id shouldBe domainExisting.id
    daoExisting.createdAt shouldBe domainExisting.createdAt
    daoExisting.updatedAt shouldBe domainExisting.updatedAt
    daoExisting.data.playerId shouldBe domainExisting.data.playerId

    daoExisting.toDomain.value shouldBe domainExisting
  }
