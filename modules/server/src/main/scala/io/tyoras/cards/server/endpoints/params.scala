package io.tyoras.cards.server.endpoints

import cats.data.NonEmptyList
import io.tyoras.cards.domain.user.model.User
import org.http4s.{ParseFailure, QueryParamDecoder, QueryParameterValue}
import io.github.iltotore.iron.cats.*

object params:
  given QueryParamDecoder[User.Name] = (param: QueryParameterValue) =>
    User.Name.validated(param.value).leftMap(e => NonEmptyList.one(ParseFailure(s"Invalid user name", s"User name '${param.value}' is invalid: $e")))
