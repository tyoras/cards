package io.tyoras.cards.domain.auth.model

import scala.util.control.NoStackTrace

enum AuthError(val message: String) extends Exception(message) with NoStackTrace:
  case InvalidToken(detail: String)    extends AuthError(s"Invalid token: $detail")
  case UnknownUser(userName: UserName) extends AuthError(s"$userName is unknown")
