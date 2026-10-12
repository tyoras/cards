package io.tyoras.cards.domain.auth.model

import io.tyoras.cards.domain.user.model.User

import scala.util.control.NoStackTrace

enum AuthError(val message: String) extends Exception(message) with NoStackTrace:
  case InvalidToken(detail: String)         extends AuthError(s"Invalid token: $detail")
  case UnknownUser(user: User.Name)         extends AuthError(s"$user is unknown")
  case InvalidUserName(userName: User.Name) extends AuthError(s"Invalid user name: $userName")

enum UserAuthError(val message: String) extends Exception(message) with NoStackTrace:
  case UserCreationFailure           extends UserAuthError(s"Failed to create the user")
  case CredentialPersistenceFailure  extends UserAuthError(s"Failed to persist the user credentials")
  case InvalidHash(detail: String)   extends UserAuthError(s"Invalid password hash generated: $detail")
  case InvalidSalt(detail: String)   extends UserAuthError(s"Invalid password salt generated: $detail")
  case UserNotFound(userId: User.ID) extends UserAuthError(s"User with id $userId does not exist")
  case InvalidOldPassword            extends UserAuthError("User old password hash does not match current password hash")
