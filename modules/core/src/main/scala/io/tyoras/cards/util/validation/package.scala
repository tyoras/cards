package io.tyoras.cards.util

import cats.ApplicativeThrow
import cats.data.Validated._
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.syntax.all._
import io.tyoras.cards.util.validation.BasicValidation.isMandatory
import io.tyoras.cards.util.validation.error.{ErrorField, ValidationError}

package object validation {

  type ValidationResult[A] = ValidatedNec[ErrorField, A]

  implicit class ValidationOps[A](a: A) {

    def validate[B](parentField: Option[ParentField] = None)(implicit v: Validator[A, B]): ValidationResult[B] =
      v.validate(a)(parentField)

    def validateE[B](implicit v: Validator[A, B]): Either[ValidationError, B] =
      v.validate(a).toEither.leftMap { c =>
        ValidationError(c.toList)
      }

    def validateF[F[_] : ApplicativeThrow, B](implicit v: Validator[A, B]): F[B] =
      ApplicativeThrow[F].fromEither(validateE)
  }

  implicit class FieldValidationOps[A](a: A)(implicit pf: Option[ParentField] = None) {
    /** Apply validators on an optional field. Use it for optional field with default value.
      *
      * @param field
      *   field name
      * @param validators
      *   validation functions to apply on the field value
      * @return
      *   ValidationResult containing all the failed validations in case of failures or the value itself in case of success
      */
    def optional(field: String, validators: ((String, A) => ValidationResult[A])*): ValidationResult[A] =
      validateField(field, a, validators: _*)

    def nestedOptional[B](field: String)(implicit v: Validator[A, B]): ValidationResult[B] = {
      val fullFieldName = completeFieldName(field)
      a.validate(ParentField(fullFieldName).some)
    }
  }

  implicit class MandatoryFieldValidationOps[A](a: Option[A])(implicit pf: Option[ParentField] = None) {

    /** Apply validators on a mandatory field after checking it is present. Use it for mandatory field.
      *
      * @param field
      *   field name
      * @param validators
      *   validation functions to apply on the field value
      * @return
      *   ValidationResult containing all the failed validations in case of failures or the value itself in case of success
      */
    def mandatory(field: String, validators: ((String, A) => ValidationResult[A])*): ValidationResult[A] = isMandatory(completeFieldName(field), a) andThen {
      v => validateField(field, v, validators: _*)
    }

    /** Apply validators on an optional field if it present. Use it for optional field without default value.
      *
      * @param field
      *   field name
      * @param validators
      *   validation functions to apply on the field value
      * @return
      *   ValidationResult containing all the failed validations in case of failures or the value itself in case of success
      */
    def optional(field: String, validators: ((String, A) => ValidationResult[A])*): ValidationResult[Option[A]] =
      a.fold(none[A].valid[NonEmptyChain[ErrorField]])(v => validateField(field, v, validators: _*).map(_.some))

    def nestedMandatory[B](field: String)(implicit v: Validator[A, B]): ValidationResult[B] = {
      val fullFieldName = completeFieldName(field)
      isMandatory(fullFieldName, a) andThen { _.validate(ParentField(fullFieldName).some) }
    }

    def nestedOptional[B](field: String)(implicit v: Validator[A, B]): ValidationResult[Option[B]] =
      a.fold(none[B].valid[NonEmptyChain[ErrorField]])(_.validate(ParentField(completeFieldName(field)).some).map(_.some))
  }

  private def validateField[A](field: String, value: A, validators: ((String, A) => ValidationResult[A])*)(implicit
    pf: Option[ParentField]
  ) =
    validators.toList.map(_.apply(completeFieldName(field), value)).sequence_.map { _ => value }

  private def completeFieldName(field: String)(implicit parentField: Option[ParentField]) =
    parentField.map(pf => s"${pf.name}.$field").getOrElse(field)
}
