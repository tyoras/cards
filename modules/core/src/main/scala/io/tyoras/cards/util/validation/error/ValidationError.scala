package io.tyoras.cards.util.validation.error

case class ValidationError(
  errors: List[ErrorField],
  code: String = "validation_error",
  message: String = "Validation failed"
) extends Exception(message)
