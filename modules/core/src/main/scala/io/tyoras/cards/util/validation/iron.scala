package io.tyoras.cards.util.validation

import io.github.iltotore.iron.cats.*
import cats.implicits.catsSyntaxOptionId
import io.github.iltotore.iron.*
import io.github.iltotore.iron.RefinedType.Mirror
import io.tyoras.cards.util.validation.error.ErrorField

object iron:
  final case class IronFieldError(fieldName: String, msg: String) extends ErrorField:
    override def code = "invalid_field"

    override def field: String = fieldName

    override def message: Option[String] = s"$fieldName is invalid : $msg".some

  /** Validator for refined types. It uses the iron library to validate the value and return a ValidationResult.
    * @param mirror
    *   evidence that the type T is a refined type
    * @tparam T
    *   the refined type to validate
    * @return
    *   a Validator that validates and transforms a value of the base type to the refined type T, returning a ValidationResult
    */
  given [T](using mirror: RefinedType.Mirror[T]): Validator[mirror.BaseType, T] = new:
    override def validate(value: mirror.BaseType)(using pf: Option[ParentField]): ValidationResult[mirror.FinalType] =
      isValid[T](pf.map(_.name).getOrElse("value"), value)

  private def isValid[T](using mirror: RefinedType.Mirror[T])(field: String, value: mirror.BaseType): ValidationResult[T] =
    mirror.ops.validatedNec(value).leftMap(_.map(IronFieldError(field, _))).map(_.asInstanceOf[mirror.FinalType])
