package io.tyoras.cards.tests.endpoint.users

import io.chrisdavenport.fuuid.FUUID
import io.circe.parser.parse
import io.circe.syntax.*
import io.scalaland.chimney.Transformer
import io.tyoras.cards.domain.user.User
import io.tyoras.cards.shared.endpoint.users.Payloads.{Request, Response}
import io.tyoras.cards.util.validation.BasicValidation.MissingFieldError
import io.tyoras.cards.util.validation.StringValidation.{BlankFieldError, TooLongError}
import io.tyoras.cards.util.validation.syntax.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime
import java.util.UUID

class PayloadsSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val now        = ZonedDateTime.now()
  private val testUserId = FUUID.fromUUID(UUID.randomUUID())

  "Creation validator" should "validate successfully when name and about are provided" in {
    val creation = Request.Creation(name = Some("John Doe"), about = Some("A test user"))
    val result   = creation.validateE[User.Data]

    result should be(Right(User.Data("John Doe", "A test user")))
  }

  it should "return missing name error when name is absent" in {
    val creation = Request.Creation(name = None, about = Some("A test user"))
    val result   = creation.validateE[User.Data]

    result.left.value.errors should contain(MissingFieldError("name"))
  }

  it should "return missing about error when about is absent" in {
    val creation = Request.Creation(name = Some("John Doe"), about = None)
    val result   = creation.validateE[User.Data]

    result.left.value.errors should contain(MissingFieldError("about"))
  }

  it should "return both missing errors when name and about are absent" in {
    val creation = Request.Creation(name = None, about = None)
    val result   = creation.validateE[User.Data]

    (result.left.value.errors should contain).`allOf`(MissingFieldError("name"), MissingFieldError("about"))
  }

  it should "return blank name error when name is blank" in {
    val creation = Request.Creation(name = Some("   "), about = Some("A test user"))
    val result   = creation.validateE[User.Data]

    result.left.value.errors should contain(BlankFieldError("name"))
  }

  it should "return blank about error when about is blank" in {
    val creation = Request.Creation(name = Some("John Doe"), about = Some("   "))
    val result   = creation.validateE[User.Data]

    result.left.value.errors should contain(BlankFieldError("about"))
  }

  it should "return blank errors for both fields when both are blank" in {
    val creation = Request.Creation(name = Some(""), about = Some(""))
    val result   = creation.validateE[User.Data]

    (result.left.value.errors should contain).`allOf`(BlankFieldError("name"), BlankFieldError("about"))
  }

  it should "return name too long error when name exceeds max length" in {
    val longName = "a" * 101
    val creation = Request.Creation(name = Some(longName), about = Some("A test user"))
    val result   = creation.validateE[User.Data]

    result.left.value.errors should contain(TooLongError("name", 100))
  }

  it should "validate successfully when name is exactly at max length" in {
    val maxName  = "a" * 100
    val creation = Request.Creation(name = Some(maxName), about = Some("About"))
    val result   = creation.validateE[User.Data]

    result should be(Right(User.Data(maxName, "About")))
  }

  it should "validate successfully when name contains special characters" in {
    val creation = Request.Creation(name = Some("John O'Donnell-Smith"), about = Some("A user with special chars"))
    val result   = creation.validateE[User.Data]

    result should be(Right(User.Data("John O'Donnell-Smith", "A user with special chars")))
  }

  it should "validate successfully when name and about contain unicode characters" in {
    val creation = Request.Creation(name = Some("Jean-François"), about = Some("Über cool user"))
    val result   = creation.validateE[User.Data]

    result should be(Right(User.Data("Jean-François", "Über cool user")))
  }

  it should "validate successfully when about is very long" in {
    val longAbout = "This is a very long description about the user. " * 10
    val creation  = Request.Creation(name = Some("John"), about = Some(longAbout))
    val result    = creation.validateE[User.Data]

    result.isRight should be(true)
  }

  it should "preserve whitespace in the middle of name" in {
    val creation = Request.Creation(name = Some("Jean Claude"), about = Some("A user"))
    val result   = creation.validateE[User.Data]

    result should be(Right(User.Data("Jean Claude", "A user")))
  }

  it should "preserve leading and trailing whitespace in about" in {
    val creation = Request.Creation(name = Some("John"), about = Some("  About text  "))
    val result   = creation.validateE[User.Data]

    result should be(Right(User.Data("John", "  About text  ")))
  }

  "Creation request decoder" should "decode snake_case JSON to Creation case class" in {
    val jsonString = """{"name": "John Doe", "about": "A test user"}"""
    val decoded    = parse(jsonString).flatMap(_.as[Request.Creation])

    decoded.toOption.get should be(Request.Creation(Some("John Doe"), Some("A test user")))
  }

  it should "handle missing name field" in {
    val jsonString = """{"about": "A test user"}"""
    val decoded    = parse(jsonString).flatMap(_.as[Request.Creation])

    decoded.toOption.get should be(Request.Creation(None, Some("A test user")))
  }

  it should "handle missing about field" in {
    val jsonString = """{"name": "John Doe"}"""
    val decoded    = parse(jsonString).flatMap(_.as[Request.Creation])

    decoded.toOption.get should be(Request.Creation(Some("John Doe"), None))
  }

  it should "handle empty JSON object" in {
    val jsonString = """{}"""
    val decoded    = parse(jsonString).flatMap(_.as[Request.Creation])

    decoded.toOption.get should be(Request.Creation(None, None))
  }

  "Response User encoder" should "encode Response.User with snake_case field names" in {
    val responseUser = Response.User(testUserId, now, now, "Jane", "About Jane")
    val encoded      = responseUser.asJson

    encoded.hcursor.get[String]("created_at").isRight should be(true)
    encoded.hcursor.get[String]("updated_at").isRight should be(true)
  }

  "Transformer from Existing to Response.User" should "transform Existing user to Response.User" in {
    val userData     = User.Data("John Doe", "A test user")
    val existingUser = User.Existing(testUserId, now, now, userData)

    val transformer  = summon[Transformer[User.Existing, Response.User]]
    val responseUser = transformer.transform(existingUser)

    responseUser should be(Response.User(testUserId, now, now, "John Doe", "A test user"))
  }

  it should "extract name from user data" in {
    val userData     = User.Data("Alice", "Alice's bio")
    val existingUser = User.Existing(testUserId, now, now, userData)

    val transformer  = summon[Transformer[User.Existing, Response.User]]
    val responseUser = transformer.transform(existingUser)

    responseUser.name should be("Alice")
  }

  it should "extract about from user data" in {
    val userData     = User.Data("Bob", "Bob's bio")
    val existingUser = User.Existing(testUserId, now, now, userData)

    val transformer  = summon[Transformer[User.Existing, Response.User]]
    val responseUser = transformer.transform(existingUser)

    responseUser.about should be("Bob's bio")
  }

  it should "preserve user id during transformation" in {
    val userId       = FUUID.fromUUID(UUID.randomUUID())
    val userData     = User.Data("Test", "Test bio")
    val existingUser = User.Existing(userId, now, now, userData)

    val transformer  = summon[Transformer[User.Existing, Response.User]]
    val responseUser = transformer.transform(existingUser)

    responseUser.id should be(userId)
  }

  it should "preserve createdAt timestamp during transformation" in {
    val createdAt    = ZonedDateTime.now().minusDays(1)
    val userData     = User.Data("Test", "Bio")
    val existingUser = User.Existing(testUserId, createdAt, now, userData)

    val transformer  = summon[Transformer[User.Existing, Response.User]]
    val responseUser = transformer.transform(existingUser)

    responseUser.createdAt should be(createdAt)
  }

  it should "preserve updatedAt timestamp during transformation" in {
    val updatedAt    = ZonedDateTime.now()
    val userData     = User.Data("Test", "Bio")
    val existingUser = User.Existing(testUserId, now, updatedAt, userData)

    val transformer  = summon[Transformer[User.Existing, Response.User]]
    val responseUser = transformer.transform(existingUser)

    responseUser.updatedAt should be(updatedAt)
  }

  it should "transform user with special characters in name and about" in {
    val userData     = User.Data("François Müller", "Über cool bio with émojis")
    val existingUser = User.Existing(testUserId, now, now, userData)

    val transformer  = summon[Transformer[User.Existing, Response.User]]
    val responseUser = transformer.transform(existingUser)

    responseUser.name should be("François Müller")
    responseUser.about should be("Über cool bio with émojis")
  }
