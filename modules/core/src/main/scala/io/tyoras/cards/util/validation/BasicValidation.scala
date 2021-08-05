package io.tyoras.cards.util.validation

import cats.syntax.option._
import io.tyoras.cards.util.validation.error.ErrorField

object BasicValidation {

  final case class MissingFieldError(fieldName: String) extends ErrorField {
    override def code = "not_found"

    override def field: String = fieldName

    override def message: Option[String] = s"$fieldName not found.".some
  }

  def isMandatory[T](field: String, value: Option[T]): ValidationResult[T] =
    value.toValidNec(MissingFieldError(field))

}
