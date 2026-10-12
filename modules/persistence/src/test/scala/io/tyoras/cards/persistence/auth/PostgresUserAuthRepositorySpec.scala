package io.tyoras.cards.persistence.auth

import cats.effect.IO
import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.auth.UserAuthRepository
import io.tyoras.cards.domain.auth.model.UserAuth
import io.tyoras.cards.domain.user.UserRepository
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.persistence.auth.PostgresUserAuthRepositorySpec.{authData, existingPlayer, otherAuthData, otherPlayerId, playerId}
import io.tyoras.cards.persistence.{PersistenceError, PgIntegrationTest}
import io.tyoras.cards.persistence.user.PostgresUserRepository
import org.scalatest.BeforeAndAfterEach
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.time.ZonedDateTime
import java.util.UUID

object PostgresUserAuthRepositorySpec:
  private val playerId       = FUUID.fromUUID(UUID.randomUUID())
  private val otherPlayerId  = FUUID.fromUUID(UUID.randomUUID())
  private val playerData     = User.Data(User.Name("Julio"), User.About("Cool cat"))
  private val existingPlayer = User.Existing(playerId, ZonedDateTime.now(), ZonedDateTime.now(), playerData)
  private val authData       = UserAuth.Data(
    playerId,
    UserAuth.PasswordHashBytes.assume(Array.ofDim(32)),
    UserAuth.PasswordSaltBytes.assume(Array.ofDim(16))
  )
  private val otherAuthData = UserAuth.Data(
    otherPlayerId,
    UserAuth.PasswordHashBytes.assume(Array.fill(32)(1.toByte)),
    UserAuth.PasswordSaltBytes.assume(Array.fill(16)(1.toByte))
  )

class PostgresUserAuthRepositorySpec extends PgIntegrationTest with BeforeAndAfterEach:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def userRepo: IO[UserRepository[IO]] = PostgresUserRepository.of(sessionPool)

  private def initPgRepo: IO[UserAuthRepository[IO]] = PostgresUserAuthRepository.of(sessionPool)

  override protected def beforeEach(): Unit =
    // cascading delete will remove everything else
    userRepo.flatMap(_.deleteAll).unsafeRunSync()

  "PostgresUserAuthRepository" when {
    "inserting user auth" when {
      "the auth does not already exist" should {
        "be able to be created" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo.flatMap(_.insert(authData)).asserting { created =>
              created.data.userId shouldBe authData.userId
              (created.data.passwordHash should contain).theSameElementsInOrderAs(authData.passwordHash)
              (created.data.passwordSalt should contain).theSameElementsInOrderAs(authData.passwordSalt)
              created.createdAt shouldBe created.updatedAt
            }
        }
      }

      "the auth already exists" should {
        "return an error" in {
          for
            testedUserRepo <- userRepo
            _              <- testedUserRepo.insert(existingPlayer.data, withId = existingPlayer.id.some)
            testedAuthRepo <- initPgRepo
            _              <- testedAuthRepo.insert(authData)
            result         <- testedAuthRepo.insert(authData).attempt
          yield result shouldBe Left(PersistenceError("already_exist", "User auth already exist"))
        }
      }
    }

    "updating user auth" should {
      "update its password hash and timestamp" in {
        val updatedHash = UserAuth.PasswordHashBytes.assume(Array.fill(32)(2.toByte))

        for
          testedUserRepo <- userRepo
          _              <- testedUserRepo.insert(existingPlayer.data, withId = existingPlayer.id.some)
          testedAuthRepo <- initPgRepo
          created        <- testedAuthRepo.insert(authData)
          updated        <- testedAuthRepo.update(created.withUpdatedHash(updatedHash))
        yield
          (updated.data.passwordHash should contain).theSameElementsInOrderAs(updatedHash)
          (updated.data.passwordSalt should contain).theSameElementsInOrderAs(authData.passwordSalt)
          updated.createdAt shouldBe created.createdAt
          updated.updatedAt.isAfter(created.updatedAt) shouldBe true
      }
    }

    "reading user auth by user id" when {
      "the auth exists" should {
        "return it" in {
          for
            testedUserRepo <- userRepo
            _              <- testedUserRepo.insert(existingPlayer.data, withId = existingPlayer.id.some)
            testedAuthRepo <- initPgRepo
            created        <- testedAuthRepo.insert(authData)
            read           <- testedAuthRepo.readByUserId(playerId)
          yield
            read.map(_.createdAt) shouldBe Some(created.createdAt)
            read.map(_.updatedAt) shouldBe Some(created.updatedAt)
            read.map(_.data.userId) shouldBe Some(created.data.userId)
            read.map(_.data.passwordHash.toSeq) shouldBe Some(created.data.passwordHash.toSeq)
            read.map(_.data.passwordSalt.toSeq) shouldBe Some(created.data.passwordSalt.toSeq)
        }
      }

      "the auth does not exist" should {
        "return none" in
          initPgRepo.flatMap(_.readByUserId(playerId)).asserting(_ shouldBe None)
      }
    }

    "deleting user auth" should {
      "remove the auth for that user" in {
        for
          testedUserRepo <- userRepo
          _              <- testedUserRepo.insert(existingPlayer.data, withId = existingPlayer.id.some)
          testedAuthRepo <- initPgRepo
          created        <- testedAuthRepo.insert(authData)
          _              <- testedAuthRepo.delete(created)
          read           <- testedAuthRepo.readByUserId(playerId)
        yield read shouldBe None
      }
    }

    "deleting all user auth" should {
      "remove every auth record" in {
        for
          testedUserRepo <- userRepo
          _              <- testedUserRepo.insert(existingPlayer.data, withId = existingPlayer.id.some)
          _              <- testedUserRepo.insert(User.Data(User.Name("Tyrion"), User.About("Great cat")), withId = otherPlayerId.some)
          testedAuthRepo <- initPgRepo
          _              <- testedAuthRepo.insert(authData)
          _              <- testedAuthRepo.insert(otherAuthData)
          _              <- testedAuthRepo.deleteAll
          first          <- testedAuthRepo.readByUserId(playerId)
          second         <- testedAuthRepo.readByUserId(otherPlayerId)
        yield
          first shouldBe None
          second shouldBe None
      }
    }
  }
