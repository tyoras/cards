package io.tyoras.cards.server.endpoints.utils

import dev.profunktor.auth.jwt.JwtToken
import io.tyoras.cards.domain.auth.model.Password
import io.tyoras.cards.domain.user.model.User

import java.time.ZonedDateTime
import java.util.UUID

object TestData:

  val now: ZonedDateTime = ZonedDateTime.now()

  val existingUserPassword: Password  = Password("password")
  val existingUserAuthToken: JwtToken = JwtToken("test_user_auth_token....")
  val existingUser: User.Existing     =
    User.Existing(User.ID(UUID.randomUUID()), now, now, User.Data(User.Name("test"), User.About("Existing user")))
