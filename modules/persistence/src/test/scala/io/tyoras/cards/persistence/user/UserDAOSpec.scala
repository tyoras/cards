package io.tyoras.cards.persistence.user

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.tyoras.cards.persistence.user.UserDAO
import io.tyoras.cards.domain.user.model.User
import io.chrisdavenport.fuuid.FUUID

import java.time.ZonedDateTime
import java.util.UUID
import io.github.iltotore.iron.*
import org.scalatest.EitherValues
import UserDAOSpec.*

object UserDAOSpec:
  private val id             = FUUID.fromUUID(UUID.randomUUID())
  private val now            = ZonedDateTime.now()
  private val domainData     = User.Data(User.Name("Julio"), User.About("Cool cat"))
  private val daoData        = UserDAO.Data.fromDomain(domainData)
  private val domainExisting = User.Existing(id, now, now, domainData)
  private val daoExisting    = UserDAO.Existing.fromDomain(domainExisting)

class UserDAOSpec extends AnyFlatSpec with Matchers with EitherValues:

  "UserDAO.Data" should "convert from and to domain Data" in {
    daoData.name shouldBe domainData.name
    daoData.about shouldBe domainData.about

    daoData.toDomain.value shouldBe domainData
  }

  "UserDAO.Existing" should "convert from and to domain Existing" in {
    daoExisting.id shouldBe domainExisting.id
    daoExisting.createdAt shouldBe domainExisting.createdAt
    daoExisting.updatedAt shouldBe domainExisting.updatedAt
    daoExisting.data shouldBe UserDAO.Data.fromDomain(domainExisting.data)

    daoExisting.toDomain.value shouldBe domainExisting
  }
