package io.tyoras.cards.tests.util.validation

import cats.syntax.validated.*
import io.tyoras.cards.util.validation.string.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StringValidationSpec extends AnyFlatSpec with Matchers:

  "notEmpty validator" should "return EmptyFieldError when the field is empty" in {
    val fieldName     = "field_name"
    val expectedError = EmptyFieldError(fieldName)
    notEmpty(fieldName, "") should be(expectedError.invalidNec)
  }

  it should "return the field value when the field is not empty" in {
    val fieldValue = "hello"
    notEmpty("field_name", fieldValue) should be(fieldValue.validNec)
  }

  it should "return the field value when the field is blank" in {
    val fieldValue = "   "
    notEmpty("field_name", fieldValue) should be(fieldValue.validNec)
  }

  "notBlank validator" should "return BlankFieldError when the field is blank" in {
    val fieldName     = "field_name"
    val expectedError = BlankFieldError(fieldName)
    notBlank(fieldName, "   ") should be(expectedError.invalidNec)
  }

  it should "return the field value when the field is not blank" in {
    val fieldValue = "  hello  "
    notBlank("field_name", fieldValue) should be(fieldValue.validNec)
  }

  it should "return BlankFieldError when the field is empty" in {
    val fieldName     = "field_name"
    val expectedError = BlankFieldError(fieldName)
    notBlank(fieldName, "") should be(expectedError.invalidNec)
  }

  "max validator" should "return TooLongError when the size of the field is greater than the max length" in {
    val fieldName     = "field_name"
    val maxLength     = 3
    val expectedError = TooLongError(fieldName, maxLength)
    max(maxLength)(fieldName, "abcdefg") should be(expectedError.invalidNec)
  }

  it should "return the field value when the size of the field is lower than the max length" in {
    val fieldValue = "ab"
    max(3)("field_name", fieldValue) should be(fieldValue.validNec)
  }

  it should "return the field value when the size of the field is equal to the max length" in {
    val fieldValue = "abc"
    max(3)("field_name", fieldValue) should be(fieldValue.validNec)
  }

  "min validator" should "return TooShortError when the size of the field is lower than the min length" in {
    val fieldName     = "field_name"
    val minLength     = 3
    val expectedError = TooShortError(fieldName, minLength)
    min(minLength)(fieldName, "ab") should be(expectedError.invalidNec)
  }

  it should "return the field value when the size of the field is greater than the min length" in {
    val fieldValue = "abcd"
    min(3)("field_name", fieldValue) should be(fieldValue.validNec)
  }

  it should "return the field value when the size of the field is equal to the min length" in {
    val fieldValue = "abc"
    min(3)("field_name", fieldValue) should be(fieldValue.validNec)
  }

  "EmptyFieldError" should "expose its code, field and message" in {
    val error = EmptyFieldError("name")
    (error.code, error.field, error.message) should be(("empty", "name", Some("name should not be empty.")))
  }

  "BlankFieldError" should "expose its code, field and message" in {
    val error = BlankFieldError("name")
    (error.code, error.field, error.message) should be(("blank", "name", Some("name should not be blank.")))
  }

  "TooShortError" should "expose its code, field and message" in {
    val error = TooShortError("name", 3)
    (error.code, error.field, error.message) should be(("too_short", "name", Some("name is too short, the minimum authorized length is 3.")))
  }

  "TooLongError" should "expose its code, field and message" in {
    val error = TooLongError("name", 3)
    (error.code, error.field, error.message) should be(("too_long", "name", Some("name is too long, the maximum authorized length is 3.")))
  }
