package io.tyoras.cards.shared.endpoint

import io.circe.Codec

object ErrorPayloads:
  object Response:
    final case class ApiMessage(code: String, message: String, errors: List[ApiFieldError] = Nil) derives Codec
    final case class ApiFieldError(code: String, field: String, message: String) derives Codec
