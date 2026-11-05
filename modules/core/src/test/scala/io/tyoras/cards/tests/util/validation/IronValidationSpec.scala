package io.tyoras.cards.tests.util.validation

import cats.syntax.all.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.tests.util.validation.ValidationSpec.{FakeDomainObject, FakeDomainSubObject, FakeInput, FakeInputSubObject, fakeInputValidator}
import io.tyoras.cards.util.validation.BasicValidation.MissingFieldError
import io.tyoras.cards.util.validation.{ParentField, ValidationResult}
import io.tyoras.cards.util.validation.iron.IronFieldError
import io.tyoras.cards.util.validation.iron.given
import io.tyoras.cards.util.validation.string.notBlank
import io.tyoras.cards.util.validation.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IronValidationSpec extends AnyFlatSpec with Matchers:

  "The iron validator" should "accept a valid value" in {
    val result: ValidationResult[User.Name] = "Alice".validate[User.Name]
    result shouldBe User.Name("Alice").validNec
  }

  it should "name the field 'value' when it has no parent field" in {
    val result: ValidationResult[User.Name] = "".validate[User.Name]
    result.leftMap(_.map(_.field).toList) shouldBe List("value").invalid
  }

  it should "name the field after its parent" in {
    // explicit because relying on the default using parameter here crashes the Scala 3.9.0 compiler
    given Option[ParentField] = None
    val result                = "".some.nestedMandatory[User.Name]("name")
    result.leftMap(_.map(_.field).toList) shouldBe List("name").invalid
  }

  "IronFieldError" should "expose its code, field and message" in {
    val error = IronFieldError("name", "too long")
    error.code shouldBe "invalid_field"
    error.field shouldBe "name"
    error.message shouldBe Some("name is invalid : too long")
  }

  "validate" should "return all the errors, without any parent field by default" in {
    val input = FakeInput(None, None, None, None)
    input.validate[FakeDomainObject].leftMap(_.map(_.field).toList) shouldBe List("mandatory_field", "mandatory_object").invalid
  }

  "nestedOptional" should "validate a nested object when it is present" in {
    val input = FakeInput("abcd".some, None, FakeInputSubObject(2.some, None).some, FakeInputSubObject(1.some, None).some)
    input.validateE[FakeDomainObject].map(_.optional_object) shouldBe FakeDomainSubObject(2, None).some.asRight
  }

  it should "report the errors of the nested object with its full name" in {
    val input = FakeInput("abcd".some, None, FakeInputSubObject(None, None).some, FakeInputSubObject(1.some, None).some)
    input.validateE[FakeDomainObject].leftMap(_.errors) shouldBe List(MissingFieldError("optional_object.mandatory_object_field")).asLeft
  }

  "mandatory" should "work without any parent field" in {
    "a".some.mandatory("field", notBlank) shouldBe "a".validNec
    none[String].mandatory("field", notBlank) shouldBe MissingFieldError("field").invalidNec
  }

  "optional" should "work without any parent field" in {
    "a".some.optional("field", notBlank) shouldBe "a".some.validNec
    none[String].optional("field", notBlank) shouldBe none[String].validNec
  }
