package io.tyoras.cards.tests.endpoint.auth

import io.circe.parser.parse
import io.circe.syntax.*
import io.tyoras.cards.domain.auth.LoginAttempt
import io.tyoras.cards.shared.endpoint.auth.Payloads.Request
import io.tyoras.cards.util.validation.BasicValidation.MissingFieldError
import io.tyoras.cards.util.validation.StringValidation.{BlankFieldError, TooLongError}
import io.tyoras.cards.util.validation.syntax.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PayloadsSpec extends AnyFlatSpec with Matchers with EitherValues:

  "Login validator" should "validate successfully when both username and password are provided" in {
    val login  = Request.Login(username = Some("john"), password = Some("secret123"))
    val result = login.validateE[LoginAttempt]

    result should be(Right(LoginAttempt("john", "secret123")))
  }

  it should "return missing username error when username is absent" in {
    val login  = Request.Login(username = None, password = Some("secret123"))
    val result = login.validateE[LoginAttempt]

    result.left.value.errors should contain(MissingFieldError("username"))
  }

  it should "return missing password error when password is absent" in {
    val login  = Request.Login(username = Some("john"), password = None)
    val result = login.validateE[LoginAttempt]

    result.left.value.errors should contain(MissingFieldError("password"))
  }

  it should "return both missing errors when username and password are absent" in {
    val login  = Request.Login(username = None, password = None)
    val result = login.validateE[LoginAttempt]

    (result.left.value.errors should contain).`allOf`(MissingFieldError("username"), MissingFieldError("password"))
  }

  it should "return blank username error when username is blank" in {
    val login  = Request.Login(username = Some("   "), password = Some("secret123"))
    val result = login.validateE[LoginAttempt]

    result.left.value.errors should contain(BlankFieldError("username"))
  }

  it should "return blank password error when password is blank" in {
    val login  = Request.Login(username = Some("john"), password = Some("   "))
    val result = login.validateE[LoginAttempt]

    result.left.value.errors should contain(BlankFieldError("password"))
  }

  it should "return blank error for both fields when both are blank" in {
    val login  = Request.Login(username = Some("  "), password = Some(""))
    val result = login.validateE[LoginAttempt]

    (result.left.value.errors should contain).`allOf`(BlankFieldError("username"), BlankFieldError("password"))
  }

  it should "return username too long error when username exceeds max length" in {
    val longUsername = "a" * 101
    val login        = Request.Login(username = Some(longUsername), password = Some("secret123"))
    val result       = login.validateE[LoginAttempt]

    result.left.value.errors should contain(TooLongError("username", 100))
  }

  it should "validate successfully when username is exactly at max length" in {
    val maxUsername = "a" * 100
    val login       = Request.Login(username = Some(maxUsername), password = Some("secret123"))
    val result      = login.validateE[LoginAttempt]

    result should be(Right(LoginAttempt(maxUsername, "secret123")))
  }

  it should "validate successfully when username contains special characters" in {
    val login  = Request.Login(username = Some("user@example.com"), password = Some("pAssw0rd!"))
    val result = login.validateE[LoginAttempt]

    result should be(Right(LoginAttempt("user@example.com", "pAssw0rd!")))
  }

  it should "validate successfully when password contains whitespace in the middle" in {
    val login  = Request.Login(username = Some("john"), password = Some("pass word"))
    val result = login.validateE[LoginAttempt]

    result should be(Right(LoginAttempt("john", "pass word")))
  }

  "Login request decoder" should "decode snake_case JSON to Login case class" in {
    val jsonString = """{"username": "john", "password": "secret123"}"""
    val decoded    = parse(jsonString).flatMap(_.as[Request.Login])

    decoded.toOption.get should be(Request.Login(Some("john"), Some("secret123")))
  }

  it should "handle missing username field" in {
    val jsonString = """{"password": "secret123"}"""
    val decoded    = parse(jsonString).flatMap(_.as[Request.Login])

    decoded.toOption.get should be(Request.Login(None, Some("secret123")))
  }

  it should "handle missing password field" in {
    val jsonString = """{"username": "john"}"""
    val decoded    = parse(jsonString).flatMap(_.as[Request.Login])

    decoded.toOption.get should be(Request.Login(Some("john"), None))
  }

  it should "handle empty JSON object" in {
    val jsonString = """{}"""
    val decoded    = parse(jsonString).flatMap(_.as[Request.Login])

    decoded.toOption.get should be(Request.Login(None, None))
  }

  "Login request encoder" should "encode Login case class to snake_case JSON" in {
    val login   = Request.Login(Some("john"), Some("secret123"))
    val encoded = login.asJson

    encoded.hcursor.get[String]("username").toOption.get should be("john")
    encoded.hcursor.get[String]("password").toOption.get should be("secret123")
  }

  it should "encode Login with None values as null" in {
    val login   = Request.Login(None, None)
    val encoded = login.asJson

    encoded.hcursor.get[String]("username").toOption should be(None)
    encoded.hcursor.get[String]("password").toOption should be(None)
  }

  it should "round-trip Login encoding and decoding" in {
    val originalLogin = Request.Login(Some("testuser"), Some("testpass"))
    val encoded       = originalLogin.asJson
    val decoded       = encoded.as[Request.Login]

    decoded.toOption.get should be(originalLogin)
  }
