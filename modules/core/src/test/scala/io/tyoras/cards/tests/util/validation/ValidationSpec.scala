package io.tyoras.cards.tests.util.validation

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.validated._
import io.tyoras.cards.tests.util.validation.ValidationSpec.{FakeDomainObject, FakeDomainSubObject, FakeInput, FakeInputSubObject}
import io.tyoras.cards.util.validation.BasicValidation._
import io.tyoras.cards.util.validation.StringValidation._
import io.tyoras.cards.util.validation._
import io.tyoras.cards.util.validation.syntax._
import io.tyoras.cards.util.validation.error.ValidationError
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValidationSpec extends AnyFlatSpec with Matchers with EitherValues:

  given IORuntime = cats.effect.unsafe.IORuntime.global

  "validateE" should "return the validated object with complex input when input is valid" in {
    val input = FakeInput("abcd".some, None, None, FakeInputSubObject(1.some, None).some)
    val expected = FakeDomainObject("abcd", None, None, FakeDomainSubObject(1, None), optional_with_default_field = false)

    import io.tyoras.cards.tests.util.validation.ValidationSpec.fakeInputValidator
    input.validateE[FakeDomainObject] should be(expected.asRight)
  }

  it should "return an error when one nested object field is not valid" in {
    val input = FakeInput("abcd".some, None, None, FakeInputSubObject(None, None).some)
    val expectedError = List(MissingFieldError("mandatory_object.mandatory_object_field"))

    import io.tyoras.cards.tests.util.validation.ValidationSpec.fakeInputValidator
    input.validateE[FakeDomainObject].left.value.errors should be(expectedError)
  }

  it should "return all errors when several nested object fields are not valid" in {
    val input = FakeInput("abcd".some, None, None, FakeInputSubObject(None, "".some).some)
    val expectedError = List(
      MissingFieldError("mandatory_object.mandatory_object_field"),
      BlankFieldError("mandatory_object.optional_object_field")
    )

    import io.tyoras.cards.tests.util.validation.ValidationSpec.fakeInputValidator
    input.validateE[FakeDomainObject].left.value.errors should be(expectedError)
  }

  "validateF" should "return the validated object when input is valid" in {
    val input = FakeInput("abcd".some, None, None, FakeInputSubObject(1.some, None).some)
    val expected = FakeDomainObject("abcd", None, None, FakeDomainSubObject(1, None), optional_with_default_field = false)

    import io.tyoras.cards.tests.util.validation.ValidationSpec.fakeInputValidator
    input.validateF[IO, FakeDomainObject].unsafeRunSync() should be(expected)
  }

  it should "return an error when one nested object field is not valid" in {
    val input = FakeInput("abcd".some, None, None, FakeInputSubObject(None, None).some)
    val expectedError = List(MissingFieldError("mandatory_object.mandatory_object_field"))

    import io.tyoras.cards.tests.util.validation.ValidationSpec.fakeInputValidator
    input.validateF[IO, FakeDomainObject].attempt.unsafeRunSync().left.value.asInstanceOf[ValidationError].errors should be(expectedError)
  }

  it should "return all errors when several nested object fields are not valid" in {
    val input = FakeInput("abcd".some, None, None, FakeInputSubObject(None, "".some).some)
    val expectedError = List(
      MissingFieldError("mandatory_object.mandatory_object_field"),
      BlankFieldError("mandatory_object.optional_object_field")
    )

    import io.tyoras.cards.tests.util.validation.ValidationSpec.fakeInputValidator
    input.validateF[IO, FakeDomainObject].attempt.unsafeRunSync().left.value.asInstanceOf[ValidationError].errors should be(expectedError)
  }

object ValidationSpec:

  case class FakeInput(
    mandatory_field: Option[String],
    optional_field: Option[Int],
    optional_object: Option[FakeInputSubObject],
    mandatory_object: Option[FakeInputSubObject],
    optional_with_default_field: Boolean = false
  )

  case class FakeInputSubObject(
    mandatory_object_field: Option[Int],
    optional_object_field: Option[String]
  )

  case class FakeDomainObject(
    mandatory_field: String,
    optional_field: Option[Int],
    optional_object: Option[FakeDomainSubObject],
    mandatory_object: FakeDomainSubObject,
    optional_with_default_field: Boolean
  )

  case class FakeDomainSubObject(
    mandatory_object_field: Int,
    optional_object_field: Option[String]
  )

  given fakeInputSubObjectValidator: Validator[FakeInputSubObject, FakeDomainSubObject] = new Validator[FakeInputSubObject, FakeDomainSubObject] {
    override def validate(i: FakeInputSubObject)(using pf: Option[ParentField]): ValidationResult[FakeDomainSubObject] = (
      i.mandatory_object_field.mandatory("mandatory_object_field"),
      i.optional_object_field.optional("optional_object_field", notBlank)
    ).mapN(FakeDomainSubObject.apply)
  }

  given fakeInputValidator: Validator[FakeInput, FakeDomainObject] = new Validator[FakeInput, FakeDomainObject] {
    override def validate(i: FakeInput)(using pf: Option[ParentField]): ValidationResult[FakeDomainObject] = (
      i.mandatory_field.mandatory("mandatory_field", notBlank, min(3), max(5)),
      i.optional_field.valid,
      i.optional_object.nestedOptional[FakeDomainSubObject]("optional_object"),
      i.mandatory_object.nestedMandatory[FakeDomainSubObject]("mandatory_object"),
      i.optional_with_default_field.valid
    ).mapN(FakeDomainObject.apply)
  }
