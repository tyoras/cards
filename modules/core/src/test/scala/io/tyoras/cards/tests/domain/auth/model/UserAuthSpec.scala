package io.tyoras.cards.tests.domain.auth.model

import cats.implicits.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.auth.model.UserAuth
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime
import java.util.UUID

class UserAuthSpec extends AnyFlatSpec with Matchers:

  private val userId = FUUID.fromUUID(UUID.randomUUID())
  private val now    = ZonedDateTime.now()
  private val hash   = UserAuth.PasswordHashBytes.assume(Array[Byte](1, 2, 3))
  private val salt   = UserAuth.PasswordSaltBytes.assume(Array[Byte](4, 5, 6))

  "UserAuth.Data" should "retain its user id, password hash, and salt" in {
    val data = UserAuth.Data(userId, hash, salt)

    data.userId shouldBe userId
    data.passwordHash.toSeq shouldBe hash.toSeq
    data.passwordSalt.toSeq shouldBe salt.toSeq
  }

  it should "return a copy with an updated password hash" in {
    val data    = UserAuth.Data(userId, hash, salt)
    val newHash = UserAuth.PasswordHashBytes.assume(Array[Byte](7, 8, 9))
    val updated = data.withUpdatedHash(newHash)

    updated.passwordHash.toSeq shouldBe newHash.toSeq
    updated.userId shouldBe data.userId
    updated.passwordSalt.toSeq shouldBe data.passwordSalt.toSeq
    data.passwordHash.toSeq shouldBe hash.toSeq
  }

  it should "show its fields" in {
    val data = UserAuth.Data(userId, hash, salt)

    data.show shouldBe s"userId = $userId | passwordHash = ${new String(hash, java.nio.charset.StandardCharsets.UTF_8)} | passwordSalt = ${new String(salt, java.nio.charset.StandardCharsets.UTF_8)}"
  }

  "UserAuth.Existing" should "expose its data and timestamps" in {
    val data     = UserAuth.Data(userId, hash, salt)
    val existing = UserAuth.Existing(now, now, data)

    existing.createdAt shouldBe now
    existing.updatedAt shouldBe now
    existing.data shouldBe data
    existing.userId shouldBe userId
    existing.passwordHash.toSeq shouldBe hash.toSeq
    existing.passwordSalt.toSeq shouldBe salt.toSeq
  }

  it should "return a copy with an updated password hash" in {
    val existing = UserAuth.Existing(now, now, UserAuth.Data(userId, hash, salt))
    val newHash  = UserAuth.PasswordHashBytes.assume(Array[Byte](7, 8, 9))
    val updated  = existing.withUpdatedHash(newHash)

    updated.passwordHash.toSeq shouldBe newHash.toSeq
    updated.data.userId shouldBe userId
    updated.passwordSalt.toSeq shouldBe salt.toSeq
    updated.createdAt shouldBe now
    updated.updatedAt shouldBe now
    existing.passwordHash.toSeq shouldBe hash.toSeq
  }

  it should "show its timestamps and credential data" in {
    val existing = UserAuth.Existing(now, now, UserAuth.Data(userId, hash, salt))

    existing.show shouldBe s"created_at = $now | updated_at = $now | userId = $userId | passwordHash = ${new String(hash, java.nio.charset.StandardCharsets.UTF_8)} | passwordSalt = ${new String(salt, java.nio.charset.StandardCharsets.UTF_8)}"
  }
