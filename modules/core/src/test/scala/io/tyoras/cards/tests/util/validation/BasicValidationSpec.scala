package io.tyoras.cards.tests.util.validation

import cats.syntax.option.*
import cats.syntax.validated.*
import io.tyoras.cards.util.validation.BasicValidation.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BasicValidationSpec extends AnyFlatSpec with Matchers:

  "isMandatory validator" should "return MissingFieldError if the field is absent" in {
    val fieldName     = "field_name"
    val expectedError = MissingFieldError(fieldName)
    isMandatory(fieldName, None) should be(expectedError.invalidNec)
  }

  it should "return the field value if the field is present" in {
    val fieldValue = 42
    isMandatory("field_name", fieldValue.some) should be(fieldValue.validNec)
  }
