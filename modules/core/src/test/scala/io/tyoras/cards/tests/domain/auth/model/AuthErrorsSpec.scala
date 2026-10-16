package io.tyoras.cards.tests.domain.auth.model

import io.tyoras.cards.domain.auth.model.{AuthError, UserAuthError}
import io.tyoras.cards.domain.user.model.User
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID

class AuthErrorsSpec extends AnyFlatSpec with Matchers:

  private val userName = User.Name("Alice")
  private val userId   = User.ID(UUID.randomUUID())

  "AuthError" should "provide the expected message for each error" in {
    AuthError.InvalidToken("expired").getMessage shouldBe "Invalid token: expired"
    AuthError.UnknownUser(userName).getMessage shouldBe "Alice is unknown"
    AuthError.InvalidUserName(userName).getMessage shouldBe "Invalid user name: Alice"
    AuthError.LoginFailure(userId, "wrong password").getMessage shouldBe s"Failed to login user $userId: wrong password"
  }

  it should "avoid capturing a stack trace" in {
    AuthError.InvalidToken("expired").getStackTrace shouldBe empty
  }

  "UserAuthError" should "provide the expected message for each error" in {
    UserAuthError.UserCreationFailure.getMessage shouldBe "Failed to create the user"
    UserAuthError.CredentialPersistenceFailure.getMessage shouldBe "Failed to persist the user credentials"
    UserAuthError.InvalidHash("empty").getMessage shouldBe "Invalid password hash generated: empty"
    UserAuthError.InvalidSalt("empty").getMessage shouldBe "Invalid password salt generated: empty"
    UserAuthError.UserNotFound(userId).getMessage shouldBe s"User with id $userId does not exist"
    UserAuthError.InvalidOldPassword.getMessage shouldBe "User old password hash does not match current password hash"
  }

  it should "avoid capturing a stack trace" in {
    UserAuthError.InvalidOldPassword.getStackTrace shouldBe empty
  }
