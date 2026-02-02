package io.tyoras.cards.util.validation

import cats.ApplicativeThrow
import cats.data.Validated.*
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.syntax.all.*

import io.tyoras.cards.util.validation.BasicValidation.isMandatory
import io.tyoras.cards.util.validation.error.{ErrorField, ValidationError}

type ValidationResult[A] = ValidatedNec[ErrorField, A]
case class ParentField(name: String) extends AnyVal

trait Validator[A, B]:
  def validate(a: A)(using pf: Option[ParentField] = None): ValidationResult[B]

object Validator:
  def apply[A, B](v: Validator[A, B])(
      pf: Option[ParentField] = None
  ): Validator[A, B] =
    v

object syntax:
  extension [A](a: A)
    def validate[B](parentField: Option[ParentField] = None)(using v: Validator[A, B]): ValidationResult[B] =
      v.validate(a)(using parentField)

    def validateE[B](using v: Validator[A, B]): Either[ValidationError, B] =
      v.validate(a).toEither.leftMap { c =>
        ValidationError(c.toList)
      }

    def validateF[F[_] : ApplicativeThrow, B](using v: Validator[A, B]): F[B] =
      ApplicativeThrow[F].fromEither(validateE)

  extension [A](a: A)(using pf: Option[ParentField] = None)
    /** Apply validators on an optional field. Use it for optional field with default value.
      *
      * @param field
      *   field name
      * @param validators
      *   validation functions to apply on the field value
      * @return
      *   ValidationResult containing all the failed validations in case of failures or the value itself in case of success
      */
    def optionalWithDefault(field: String, validators: ((String, A) => ValidationResult[A])*): ValidationResult[A] =
      validateField(field, a, validators*)

    def nestedOptionalWithDefault[B](field: String)(using v: Validator[A, B]): ValidationResult[B] =
      val fullFieldName = completeFieldName(field)
      a.validate(ParentField(fullFieldName).some)

  extension [A](a: Option[A])(using pf: Option[ParentField] = None)
    /** Apply validators on a mandatory field after checking it is present. Use it for mandatory field.
      *
      * @param field
      *   field name
      * @param validators
      *   validation functions to apply on the field value
      * @return
      *   ValidationResult containing all the failed validations in case of failures or the value itself in case of success
      */
    def mandatory(field: String, validators: ((String, A) => ValidationResult[A])*): ValidationResult[A] =
      isMandatory(completeFieldName(field), a).andThen { v =>
        validateField(field, v, validators*)
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
      a.fold(none[A].valid[NonEmptyChain[ErrorField]])(v => validateField(field, v, validators*).map(_.some))

    def nestedMandatory[B](field: String)(using v: Validator[A, B]): ValidationResult[B] =
      val fullFieldName = completeFieldName(field)
      isMandatory(fullFieldName, a).andThen {
        _.validate(ParentField(fullFieldName).some)
      }

    def nestedOptional[B](field: String)(using v: Validator[A, B]): ValidationResult[Option[B]] =
      a.fold(none[B].valid[NonEmptyChain[ErrorField]])(
        _.validate(ParentField(completeFieldName(field)).some).map(_.some)
      )

private def validateField[A](field: String, value: A, validators: ((String, A) => ValidationResult[A])*)(using pf: Option[ParentField]) =
  validators.toList.map(_.apply(completeFieldName(field), value)).sequence_.map(_ => value)

private def completeFieldName(field: String)(using parentField: Option[ParentField]) =
  parentField.map(pf => s"${pf.name}.$field").getOrElse(field)
