package io.tyoras.cards.tests.endpoint

import io.circe.testing.golden.GoldenCodecTests
import io.tyoras.cards.tests.SharedCodecTestUtils
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FlatSpecDiscipline

import io.circe.parser.parse
import io.circe.syntax.*
import io.tyoras.cards.shared.endpoint.ErrorPayloads.Response.{ApiFieldError, ApiMessage}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ErrorPayloadsSpec extends AnyFlatSpec with Matchers with EitherValues with SharedCodecTestUtils with FlatSpecDiscipline with Configuration:

  "ApiMessage" should "have no field errors by default" in {
    ApiMessage("not_found", "Resource not found").errors shouldBe empty
  }

  it should "round-trip through JSON with field errors" in {
    val msg = ApiMessage("invalid", "Invalid payload", List(ApiFieldError("blank", "name", "name should not be blank.")))

    msg.asJson.as[ApiMessage].value shouldBe msg
  }

  it should "encode its fields" in {
    val json = ApiMessage("invalid", "Invalid payload", List(ApiFieldError("blank", "name", "msg"))).asJson.hcursor

    json.get[String]("code").value shouldBe "invalid"
    json.get[String]("message").value shouldBe "Invalid payload"
    json.downField("errors").downArray.get[String]("field").value shouldBe "name"
  }

  it should "decode a JSON payload" in {
    val decoded = parse("""{"code":"c","message":"m","errors":[{"code":"fc","field":"f","message":"fm"}]}""").flatMap(_.as[ApiMessage])

    decoded.value shouldBe ApiMessage("c", "m", List(ApiFieldError("fc", "f", "fm")))
  }

  // golden JSON codec tests
  checkAll("GoldenCodec[ApiMessage]", GoldenCodecTests[ApiMessage].goldenCodec)
