package io.tyoras.cards.util.validation.error

import scala.util.control.NoStackTrace

case class ValidationError(
    errors: List[ErrorField],
    code: String = "validation_error",
    message: String = "Validation failed"
) extends Exception(message) with NoStackTrace
