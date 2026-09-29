package io.tyoras.cards.persistence.user

import io.tyoras.cards.persistence.test.matchers.DateMatcher.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.persistence.{PersistenceError, PgIntegrationTest}
import cats.effect.IO
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.user.UserRepository
import io.tyoras.cards.persistence.user.PostgresUserRepositorySpec.*

import java.time.ZonedDateTime
import java.util.UUID
import cats.syntax.all.*
import org.scalatest.BeforeAndAfterEach
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

object PostgresUserRepositorySpec:
  private val userId    = FUUID.fromUUID(UUID.randomUUID())
  private val userData  = User.Data(User.Name("Julio"), User.About("Cool cat"))
  private val user2Data = User.Data(User.Name("Tyrion"), User.About("Great cat"))

class PostgresUserRepositorySpec extends PgIntegrationTest with BeforeAndAfterEach:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def initPgRepo: IO[UserRepository[IO]] = PostgresUserRepository.of(sessionPool)

  override protected def beforeEach(): Unit =
    initPgRepo.flatMap(_.deleteAll).unsafeRunSync()

  "PostgresUserRepository" when {
    "inserting a user" when {
      "the user does not already exist" should {
        "be able to create a single user without providing an id" in {
          initPgRepo.flatMap(_.insert(userData, withId = None)).asserting { created =>
            created.data shouldBe userData
            created.createdAt shouldBe created.updatedAt
          }
        }

        "be able to create a single user with a provided id" in {
          initPgRepo.flatMap(_.insert(userData, withId = userId.some)).asserting { created =>
            created.data shouldBe userData
            created.id shouldBe userId
            created.createdAt shouldBe created.updatedAt
          }
        }
      }

      "the user already exist" should {
        "return an error (no id provided)" in {
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.insert(userData, withId = None) *>
                testedRepo.insert(userData, withId = None)
            }
            .assertThrowsError(_ shouldBe PersistenceError("already_exist", "User already exist"))
        }

        "return an error (id provided)" in {
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.insert(userData, withId = userId.some) *>
                testedRepo.insert(userData, withId = userId.some)
            }
            .assertThrowsError(_ shouldBe PersistenceError("already_exist", "User already exist"))
        }
      }
    }
    "writing many users" when {
      "all the users do not already exist" should {
        "be able to create multiple users without providing an id" in {
          initPgRepo.flatMap(_.writeMany(List(userData, user2Data)).asserting { created =>
            created should have size 2
            created.head.data shouldBe userData
            created.last.data shouldBe user2Data
            created.head.id should not be created.last.id
          })
        }
      }
      "some users already exist but they are not passed as Existing" should {
        "return an error and the not existing should not be created" in {
          initPgRepo
            .flatMap { testedRepo =>
              testedRepo.insert(user2Data, withId = None) *>
                testedRepo.writeMany(List(userData, user2Data))
            }
            .assertThrowsError(_ shouldBe PersistenceError("already_exist", "User already exist"))
        }
      }
      "some users already exist and they are passed as Existing" should {
        "create the not existing and update the existing" in {
          for
            testedRepo <- initPgRepo
            existing   <- testedRepo.insert(userData, withId = userId.some)
            updatedExisting = existing.withUpdatedAbout(User.About("Updated about"))
            createdAndUpdated <- testedRepo.writeMany(List(updatedExisting, user2Data))
          yield
            createdAndUpdated should have size 2
            createdAndUpdated.head.data shouldBe updatedExisting.data
            createdAndUpdated.head.createdAt should beSameMillis(existing.createdAt)
            createdAndUpdated.head.updatedAt should beAfter(existing.updatedAt)
            createdAndUpdated.last.data shouldBe user2Data
            createdAndUpdated.last.id should not be createdAndUpdated.head.id
        }
      }
    }
    "reading many users" when {
      "reading by id" should {
        "return the users if they exists" in {
          for
            testedRepo    <- initPgRepo
            _             <- testedRepo.insert(userData, withId = userId.some)
            user2         <- testedRepo.insert(user2Data, withId = None)
            notExistingId <- FUUID.randomFUUID[IO]
            read          <- testedRepo.readManyById(List(userId, notExistingId, user2.id))
          yield
            read should have size 2
            read.head.id shouldBe userId
            read.head.data shouldBe userData
            read.last.id shouldBe user2.id
            read.last.data shouldBe user2Data
        }

        "return an empty list if the user does not exist" in {
          initPgRepo.flatMap(_.readManyById(List(userId))).asserting { read =>
            read shouldBe empty
          }
        }
      }
    }
    "reading many users by partial name" should {
      "return the users if they exists" in {
        for
          testedRepo <- initPgRepo
          _          <- testedRepo.insert(userData, withId = userId.some)
          user2      <- testedRepo.insert(user2Data, withId = None)
          read       <- testedRepo.readManyByPartialName(User.Name("ri"))
        yield
          read should have size 1
          read.head.id shouldBe user2.id
          read.head.data shouldBe user2Data
      }

      "return an empty list if the user does not exist" in
        initPgRepo.flatMap(_.readManyByPartialName(User.Name("x"))).asserting(_ shouldBe empty)
    }
    "reading many users by exact name" should {
      "return the users if they exists" in {
        for
          testedRepo <- initPgRepo
          _          <- testedRepo.insert(userData, withId = userId.some)
          user2      <- testedRepo.insert(user2Data, withId = None)
          read       <- testedRepo.readManyByName(List(userData.name, user2Data.name))
        yield
          read should have size 2
          read.head.id shouldBe userId
          read.head.data shouldBe userData
          read.last.id shouldBe user2.id
          read.last.data shouldBe user2Data
      }

      "return an empty list if the user does not exist" in
        initPgRepo.flatMap(_.readManyByName(List(User.Name("x")))).asserting(_ shouldBe empty)
    }
    "Streaming all users" should {
      "return all the users" in {
        for
          testedRepo <- initPgRepo
          _          <- testedRepo.insert(userData, withId = userId.some)
          user2      <- testedRepo.insert(user2Data, withId = None)
          read       <- testedRepo.readAll.compile.toList
        yield
          read should have size 2
          read.head.id shouldBe userId
          read.head.data shouldBe userData
          read.last.id shouldBe user2.id
          read.last.data shouldBe user2Data
      }

      "return an empty list if there is no user" in
        initPgRepo.flatMap(_.readAll.compile.toList).asserting(_ shouldBe empty)
    }
    "deleting many users" when {
      "the users exist" should {
        "delete the users" in {
          for
            testedRepo <- initPgRepo
            existing1  <- testedRepo.insert(userData, withId = userId.some)
            existing2  <- testedRepo.insert(user2Data, withId = None)
            _          <- testedRepo.deleteMany(List(existing1, existing2))
            read       <- testedRepo.readManyById(List(existing1.id, existing2.id))
          yield read shouldBe empty
        }
      }
    }
    "deleting all users" when {
      "there are users" should {
        "delete all the users" in {
          for
            testedRepo <- initPgRepo
            _          <- testedRepo.insert(userData, withId = userId.some)
            _          <- testedRepo.insert(user2Data, withId = None)
            _          <- testedRepo.deleteAll
            read       <- testedRepo.readAll.compile.toList
          yield read shouldBe empty
        }
      }
    }
  }
