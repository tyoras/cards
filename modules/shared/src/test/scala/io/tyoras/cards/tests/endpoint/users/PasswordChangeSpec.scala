package io.tyoras.cards.tests.endpoint.users

import io.circe.parser.parse
import io.circe.syntax.*
import io.tyoras.cards.domain.auth.model.{Password, PasswordChangeRequest}
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.endpoint.users.Payloads.Request
import io.tyoras.cards.tests.endpoint.users.PayloadsSpec.expectedUserPasswordError
import io.tyoras.cards.util.validation.BasicValidation.MissingFieldError
import io.tyoras.cards.util.validation.iron.IronFieldError
import io.tyoras.cards.util.validation.syntax.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID

class PasswordChangeSpec extends AnyFlatSpec with Matchers with EitherValues:

  private given userId: User.ID = User.ID(UUID.randomUUID())

  "PasswordChange validator" should "build a PasswordChangeRequest for the given user" in {
    val result = Request.PasswordChange(Some("oldPassword"), Some("newPassword")).validateE[PasswordChangeRequest]

    result.value shouldBe PasswordChangeRequest(userId, Password("oldPassword"), Password("newPassword"))
  }

  it should "report both missing passwords" in {
    val result = Request.PasswordChange(None, None).validateE[PasswordChangeRequest]

    (result.left.value.errors should contain).allOf(MissingFieldError("old_password"), MissingFieldError("new_password"))
  }

  it should "report an invalid old password" in {
    val result = Request.PasswordChange(Some("short"), Some("newPassword")).validateE[PasswordChangeRequest]

    result.left.value.errors.toList shouldBe List(IronFieldError("old_password", expectedUserPasswordError))
  }

  it should "report an invalid new password" in {
    val result = Request.PasswordChange(Some("oldPassword"), Some("   ")).validateE[PasswordChangeRequest]

    result.left.value.errors.toList shouldBe List(IronFieldError("new_password", expectedUserPasswordError))
  }

  "PasswordChange codec" should "round-trip through JSON" in {
    val change = Request.PasswordChange(Some("old"), Some("new"))

    change.asJson.as[Request.PasswordChange].value shouldBe change
  }

  it should "decode a snake_case JSON payload" in {
    parse("""{"old_password":"a","new_password":"b"}""").flatMap(_.as[Request.PasswordChange]).value shouldBe Request.PasswordChange(Some("a"), Some("b"))
  }
