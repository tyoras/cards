package io.tyoras.cards.tests.domain.auth.model

import dev.profunktor.auth.jwt.JwtToken
import io.circe.parser.decode
import io.circe.syntax.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.tyoras.cards.domain.auth.model.*
import io.tyoras.cards.domain.user.model.User
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import pl.iterators.kebs.circe.snakified.*

import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.duration.*

class AuthModelsSpec extends AnyFlatSpec with Matchers:

  private val userId = User.ID(UUID.randomUUID())
  private val now    = ZonedDateTime.now()
  private val user   = User.Existing(
    userId,
    now,
    now,
    User.Data(User.Name("Alice"), User.About("A test user"))
  )

  "Password" should "accept a trimmed, non-blank value of at least eight characters" in {
    Password.validated("secure-pass").toEither.map(_.value) shouldBe Right("secure-pass")
  }

  it should "reject blank, short, and untrimmed values" in {
    Password.validated("        ").isInvalid shouldBe true
    Password.validated("short").isInvalid shouldBe true
    Password.validated(" secure-pass ").isInvalid shouldBe true
  }

  "TokenExpiration" should "preserve its finite duration" in {
    val expiration = TokenExpiration.assume(5.minutes)

    expiration shouldBe 5.minutes
  }

  "UserClaim" should "round-trip its user id through its Circe codec" in {
    val claim = UserClaim(userId)

    decode[UserClaim](claim.asJson.noSpaces) shouldBe Right(claim)
  }

  "LoginAttempt" should "retain the supplied username and password" in {
    val password = Password.assume("secure-pass")
    val attempt  = LoginAttempt(User.Name("Alice"), password)

    attempt.userName shouldBe User.Name("Alice")
    attempt.password shouldBe password
  }

  "LoginSuccess" should "retain the issued token and user" in {
    val token   = JwtToken("signed.jwt.token")
    val success = LoginSuccess(token, user)

    success.token shouldBe token
    success.user shouldBe user
  }

  "PasswordChangeRequest" should "retain the user id and both passwords" in {
    val oldPassword = Password.assume("old-password")
    val newPassword = Password.assume("new-password")
    val request     = PasswordChangeRequest(userId, oldPassword, newPassword)

    request.userId shouldBe userId
    request.oldPassword shouldBe oldPassword
    request.newPassword shouldBe newPassword
  }
