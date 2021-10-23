package io.tyoras.cards.tests.util.validation

import cats.data.NonEmptyChain
import cats.syntax.option._
import cats.syntax.validated._
import io.tyoras.cards.util.validation.BasicValidation.MissingFieldError
import io.tyoras.cards.util.validation.StringValidation._
import io.tyoras.cards.util.validation._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FieldValidationSpec extends AnyFlatSpec with Matchers {

  "optional with default field validator" should "return the value if it is valid" in {
    val fieldValue = "abc"
    fieldValue.optional("optional_field", max(3)) should be(fieldValue.valid)
  }

  it should "return an error if it is invalid" in {
    val fieldName = "optional_field"
    val fieldValue = "   "
    val expectedError = BlankFieldError(fieldName)
    fieldValue.optional(fieldName, notBlank) should be(expectedError.invalidNec)
  }

  it should "return all errors if several validations fail" in {
    val fieldName = "optional_field"
    val fieldValue = " "
    val minLength = 2
    val expectedErrors = NonEmptyChain(BlankFieldError(fieldName), TooShortError(fieldName, minLength))
    fieldValue.optional(fieldName, notBlank, max(3), min(minLength)) should be(expectedErrors.invalid)
  }

  "nested optional with default field validator" should "return the value if it is valid" in {
    val fieldValue = "abc"
    val nestedValidator = new Validator[String, String] {
      override def validate(a: String)(implicit pf: Option[ParentField]): ValidationResult[String] = a.optional("nested_field_name", max(3))
    }
    fieldValue.nestedOptional("optional_field")(nestedValidator) should be(fieldValue.valid)
  }

  it should "return an error if it is invalid" in {
    val fieldName = "optional_field"
    val nestedFieldName = "nested_field_name"
    val fieldValue = "   "
    val expectedError = BlankFieldError(s"$fieldName.$nestedFieldName")
    val nestedValidator = new Validator[String, String] {
      override def validate(a: String)(implicit pf: Option[ParentField]): ValidationResult[String] = a.optional(nestedFieldName, notBlank)
    }
    fieldValue.nestedOptional(fieldName)(nestedValidator) should be(expectedError.invalidNec)
  }

  it should "return all errors if several validations fail" in {
    val fieldName = "optional_field"
    val nestedFieldName = "nested_field_name"
    val fieldValue = " "
    val minLength = 2
    val completeName = s"$fieldName.$nestedFieldName"
    val expectedErrors = NonEmptyChain(BlankFieldError(completeName), TooShortError(completeName, minLength))
    val nestedValidator = new Validator[String, String] {
      override def validate(a: String)(implicit pf: Option[ParentField]): ValidationResult[String] =
        a.optional(nestedFieldName, notBlank, max(3), min(minLength))
    }
    fieldValue.nestedOptional(fieldName)(nestedValidator) should be(expectedErrors.invalid)
  }

  "optional without default field validator" should "return valid if the value is not present" in {
    val absentField = None
    absentField.optional("optional_field", max(3)) should be(none.valid)
  }

  it should "return the value wrapped in an option if the value is present and valid" in {
    val fieldValue = "abc".some
    fieldValue.optional("optional_field", max(3)) should be(fieldValue.valid)
  }

  it should "return an error if value is present and invalid" in {
    val fieldName = "optional_field"
    val fieldValue = "   ".some
    val expectedError = BlankFieldError(fieldName)
    fieldValue.optional(fieldName, notBlank) should be(expectedError.invalidNec)
  }

  it should "return all errors if value is present and several validations fail" in {
    val fieldName = "optional_field"
    val fieldValue = " ".some
    val minLength = 2
    val expectedErrors = NonEmptyChain(BlankFieldError(fieldName), TooShortError(fieldName, minLength))
    fieldValue.optional(fieldName, notBlank, max(3), min(minLength)) should be(expectedErrors.invalid)
  }

  "mandatory field validator" should "return MissingFieldError if the value is not present" in {
    val fieldName = "mandatory_field"
    val absentField = None
    val expectedError = MissingFieldError(fieldName)
    absentField.mandatory(fieldName, max(3)) should be(expectedError.invalidNec)
  }

  it should "return the value if it is valid" in {
    val fieldValue = "abc"
    fieldValue.some.mandatory("mandatory_field", max(3)) should be(fieldValue.valid)
  }

  it should "return an error if value is present and invalid" in {
    val fieldName = "mandatory_field"
    val fieldValue = "   ".some
    val expectedError = BlankFieldError(fieldName)
    fieldValue.mandatory(fieldName, notBlank) should be(expectedError.invalidNec)
  }

  it should "return all errors if value is present and several validations fail" in {
    val fieldName = "mandatory_field"
    val fieldValue = " ".some
    val minLength = 2
    val expectedErrors = NonEmptyChain(BlankFieldError(fieldName), TooShortError(fieldName, minLength))
    fieldValue.mandatory(fieldName, notBlank, max(3), min(minLength)) should be(expectedErrors.invalid)
  }
}
