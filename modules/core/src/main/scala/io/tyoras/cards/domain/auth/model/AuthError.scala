package io.tyoras.cards.domain.auth.model

import io.tyoras.cards.domain.user.model.User

import scala.util.control.NoStackTrace

enum AuthError(val message: String) extends Exception(message) with NoStackTrace:
  case InvalidToken(detail: String)         extends AuthError(s"Invalid token: $detail")
  case UnknownUser(user: User.Name)         extends AuthError(s"$user is unknown")
  case InvalidUserName(userName: User.Name) extends AuthError(s"Invalid user name: $userName")
