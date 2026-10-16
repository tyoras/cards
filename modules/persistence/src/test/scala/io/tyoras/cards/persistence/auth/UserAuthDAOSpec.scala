package io.tyoras.cards.persistence.auth

import io.tyoras.cards.domain.auth.model.UserAuth
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.persistence.auth.UserAuthDAOSpec.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime
import java.util.UUID

object UserAuthDAOSpec:
  private val userId         = User.ID(UUID.randomUUID())
  private val now            = ZonedDateTime.now()
  private val hash           = new Array[Byte](32)
  private val salt           = new Array[Byte](32)
  private val domainData     = UserAuth.Data(userId, UserAuth.PasswordHashBytes.assume(hash), UserAuth.PasswordSaltBytes.assume(salt))
  private val daoData        = UserAuthDAO.Data.fromDomain(domainData)
  private val domainExisting = UserAuth.Existing(now, now, domainData)
  private val daoExisting    = UserAuthDAO.Existing.fromDomain(domainExisting)

class UserAuthDAOSpec extends AnyFlatSpec with Matchers with EitherValues:
  "UserAuthDAO.Data" should "convert from and to domain Data" in {
    daoData.userId shouldBe domainData.userId
    (daoData.passwordSalt should contain).theSameElementsInOrderAs(domainData.passwordSalt)
    (daoData.passwordHash should contain).theSameElementsInOrderAs(domainData.passwordHash)
  }

  "UserAuthDAO.Existing" should "convert from and to domain Existing" in {
    daoExisting.createdAt shouldBe domainExisting.createdAt
    daoExisting.updatedAt shouldBe domainExisting.updatedAt
    daoExisting.data shouldBe UserAuthDAO.Data.fromDomain(domainExisting.data)

    daoExisting.toDomain.value shouldBe domainExisting
  }
