package io.tyoras.cards.tests.endpoint.auth

import dev.profunktor.auth.jwt.JwtToken
import io.circe.parser.parse
import io.circe.syntax.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.endpoint.auth.Payloads.Response
import io.tyoras.cards.shared.endpoint.auth.Payloads.Response.given
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID

class SuccessfulLoginSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val userId = User.ID(UUID.randomUUID())
  private val token  = JwtToken("header.payload.signature")

  "JwtToken codec" should "encode a token as its raw string value" in {
    token.asJson.asString shouldBe Some("header.payload.signature")
  }

  it should "decode a JSON string into a token" in {
    parse("\"abc.def.ghi\"").flatMap(_.as[JwtToken]).value shouldBe JwtToken("abc.def.ghi")
  }

  "SuccessfulLogin codec" should "encode with snake_case field names" in {
    val json = Response.SuccessfulLogin(token, userId).asJson.hcursor

    json.get[String]("token").value shouldBe "header.payload.signature"
    json.get[UUID]("user_id").value shouldBe userId.unwrap
  }

  it should "round-trip through JSON" in {
    val login = Response.SuccessfulLogin(token, userId)

    login.asJson.as[Response.SuccessfulLogin].value shouldBe login
  }
