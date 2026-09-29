package io.tyoras.cards.persistence.game.stats

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.card.Card.Count
import io.tyoras.cards.domain.game.stats.GameStatRepository
import io.tyoras.cards.domain.game.stats.model.PlayerGameStat
import io.tyoras.cards.domain.game.GameTyp
import io.tyoras.cards.domain.user.UserRepository
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.persistence.game.stats.PostgresGameStatRepositorySpec.*
import io.tyoras.cards.persistence.{PersistenceError, PgIntegrationTest}
import io.tyoras.cards.persistence.user.PostgresUserRepository
import org.scalatest.BeforeAndAfterEach
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.time.ZonedDateTime
import java.util.UUID

object PostgresGameStatRepositorySpec:
  private val playerId       = FUUID.fromUUID(UUID.randomUUID())
  private val playerData     = User.Data(User.Name("Julio"), User.About("Cool cat"))
  private val existingPlayer = User.Existing(playerId, ZonedDateTime.now(), ZonedDateTime.now(), playerData)
  private val gameType       = GameTyp.War
  private val now            = ZonedDateTime.now()
  private val statId         = FUUID.fromUUID(UUID.randomUUID())
  private val statData       = PlayerGameStat.Data(existingPlayer.id, gameType, Count(10), Count(5), Count(2))

class PostgresGameStatRepositorySpec extends PgIntegrationTest with BeforeAndAfterEach:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def initPgRepo: IO[GameStatRepository[IO]] = PostgresGameStatRepository.of(sessionPool)
  private def userRepo: IO[UserRepository[IO]]       = PostgresUserRepository.of(sessionPool)

  override protected def beforeEach(): Unit =
    // cascading delete will remove everything else
    userRepo.flatMap(repo => repo.deleteAll <* repo.insert(playerData, existingPlayer.id.some)).unsafeRunSync()

  "PostgresGameStatRepository" when {
    "inserting a game stat" when {
      "the game stat does not already exist" should {
        "be able to create a single game stat without providing an id" in {
          initPgRepo.flatMap(_.insert(statData, withId = None)).asserting { created =>
            created.data shouldBe statData
            created.createdAt shouldBe created.updatedAt
          }
        }

        "be able to create a single game stat with a provided id" in {
          initPgRepo.flatMap(_.insert(statData, withId = statId.some)).asserting { created =>
            created.data shouldBe statData
            created.id shouldBe statId
            created.createdAt shouldBe created.updatedAt
          }
        }
      }

      "the game stat already exist" should {
        "return an error (no id provided)" in {
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.insert(statData, withId = None) *>
                testedRepo.insert(statData, withId = None)
            }
            .assertThrowsError(_ shouldBe PersistenceError("already_exist", "Game stat already exist"))
        }

        "return an error (id provided)" in {
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.insert(statData, withId = statId.some) *>
                testedRepo.insert(statData, withId = statId.some)
            }
            .assertThrowsError(_ shouldBe PersistenceError("already_exist", "Game stat already exist"))
        }
      }
    }
    "writing many game stats" when {
      "the game stats do not already exist" should {
        "be able to create many game stats" in {
          val statData2    = PlayerGameStat.Data(existingPlayer.id, GameTyp.Schnapsen, Count(20), Count(10), Count(5))
          val statsToWrite = NonEmptyList.of(statData, statData2)
          initPgRepo.flatMap(_.writeMany(statsToWrite)).asserting { created =>
            created should have size 2
            created.map(_.data).toList should contain theSameElementsAs statsToWrite.toList
          }
        }
      }

      "the game stats already exist" should {
        "return an error when trying to create many game stats" in {
          val statData2    = PlayerGameStat.Data(existingPlayer.id, GameTyp.Schnapsen, Count(20), Count(10), Count(5))
          val statsToWrite = NonEmptyList.of(statData, statData2)
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.writeMany(statsToWrite) *>
                testedRepo.writeMany(statsToWrite)
            }
            .assertThrowsError(_ shouldBe PersistenceError("already_exist", "Game stat already exist"))
        }
      }
    }
    "reading a game stat" when {
      "the game stat exists" should {
        "be able to read a single game stat by player id and game type" in {
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.insert(statData, withId = None) *>
                testedRepo.readOne(existingPlayer.id, gameType)
            }
            .asserting { maybeStat =>
              maybeStat shouldBe defined
              maybeStat.get.data shouldBe statData
            }
        }
      }

      "the game stat does not exist" should {
        "return None when reading a single game stat by player id and game type" in {
          initPgRepo.flatMap(_.readOne(existingPlayer.id, gameType)).asserting { maybeStat =>
            maybeStat shouldBe None
          }
        }
      }
    }
    "reading many game stats by player" when {
      "the game stats exist" should {
        "be able to read many game stats by player id" in {
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.insert(statData, withId = None) *>
                testedRepo.readManyByPlayer(existingPlayer.id)
            }
            .asserting { stats =>
              stats should have size 1
              stats.head.data shouldBe statData
            }
        }
      }

      "the game stats do not exist" should {
        "return an empty list when reading many game stats by player id" in {
          initPgRepo.flatMap(_.readManyByPlayer(existingPlayer.id)).asserting { stats =>
            stats shouldBe empty
          }
        }
      }
    }
    "reading many game stats by ids" when {
      "the game stats exist" should {
        "be able to read many game stats by their ids" in {
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.insert(statData, withId = None).flatMap { created =>
                testedRepo.readManyById(NonEmptyList.one(created.id))
              }
            }
            .asserting { stats =>
              stats should have size 1
              stats.head.data shouldBe statData
            }
        }
      }

      "the game stats do not exist" should {
        "return an empty list when reading many game stats by their ids" in {
          initPgRepo.flatMap(_.readManyById(NonEmptyList.one(statId))).asserting { stats =>
            stats shouldBe empty
          }
        }
      }
    }
    "reading all game stats" when {
      "there are game stats" should {
        "be able to read all game stats" in {
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.insert(statData, withId = None) *>
                testedRepo.readAll.compile.toList
            }
            .asserting { stats =>
              stats should have size 1
              stats.head.data shouldBe statData
            }
        }
      }

      "there are no game stats" should {
        "return an empty list when reading all game stats" in {
          initPgRepo.flatMap(_.readAll.compile.toList).asserting { stats =>
            stats shouldBe empty
          }
        }
      }
    }
    "deleting game stats" when {
      "the game stats exist" should {
        "be able to delete many game stats by their ids" in {
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.insert(statData, withId = None).flatMap { created =>
                testedRepo.deleteMany(NonEmptyList.one(created)) *>
                  testedRepo.readManyByPlayer(existingPlayer.id)
              }
            }
            .asserting { stats =>
              stats shouldBe empty
            }
        }
      }

      "the game stats do not exist" should {
        "not fail when deleting many game stats by their ids" in {
          initPgRepo.flatMap(_.deleteMany(NonEmptyList.one(PlayerGameStat.Existing(statId, now, now, statData)))).asserting { _ =>
            succeed
          }
        }
      }
    }
    "deleting all game stats" when {
      "there are game stats" should {
        "be able to delete all game stats" in {
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.insert(statData, withId = None) *>
                testedRepo.deleteAll *>
                testedRepo.readManyByPlayer(existingPlayer.id)
            }
            .asserting { stats =>
              stats shouldBe empty
            }
        }
      }

      "there are no game stats" should {
        "not fail when deleting all game stats" in {
          initPgRepo.flatMap(_.deleteAll).asserting { _ =>
            succeed
          }
        }
      }
    }
  }
