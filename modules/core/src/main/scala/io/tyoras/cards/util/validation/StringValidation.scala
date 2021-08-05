package io.tyoras.cards.util.validation

import cats.data.Validated
import cats.syntax.option._
import io.tyoras.cards.util.validation.error.ErrorField

object StringValidation {

  final case class EmptyFieldError(fieldName: String) extends ErrorField {
    override def code = "empty"

    override def field: String = fieldName

    override def message: Option[String] = s"$fieldName should not be empty.".some
  }

  final case class BlankFieldError(fieldName: String) extends ErrorField {
    override def code = "blank"

    override def field: String = fieldName

    override def message: Option[String] = s"$fieldName should not be blank.".some
  }

  final case class TooShortError(fieldName: String, minLength: Int) extends ErrorField {
    override def code = "too_short"

    override def field: String = fieldName

    override def message: Option[String] = s"$fieldName is too short, the minimum authorized length is $minLength.".some
  }

  final case class TooLongError(fieldName: String, maxLength: Int) extends ErrorField {
    override def code = "too_long"

    override def field: String = fieldName

    override def message: Option[String] = s"$fieldName is too long, the maximum authorized length is $maxLength.".some
  }

  def notEmpty(field: String, s: String): ValidationResult[String] =
    Validated.condNec(s.nonEmpty, s, EmptyFieldError(field))

  def notBlank(field: String, s: String): ValidationResult[String] =
    Validated.condNec(s.trim.nonEmpty, s, BlankFieldError(field))

  def min(minLength: Int)(field: String, s: String): ValidationResult[String] =
    Validated.condNec(s.length >= minLength, s, TooShortError(field, minLength))

  def max(maxLength: Int)(field: String, s: String): ValidationResult[String] =
    Validated.condNec(s.length <= maxLength, s, TooLongError(field, maxLength))

}
