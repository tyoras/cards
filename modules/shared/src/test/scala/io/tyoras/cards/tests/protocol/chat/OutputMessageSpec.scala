package io.tyoras.cards.tests.protocol.chat

import io.circe.testing.golden.GoldenCodecTests
import io.tyoras.cards.tests.SharedCodecTestUtils
import org.scalatest.prop.Configuration
import io.tyoras.cards.tests.AllChatOutputMessages
import org.typelevel.discipline.scalatest.FlatSpecDiscipline

import cats.syntax.all.*
import io.circe.syntax.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.protocol.chat.OutputMessage.*
import io.tyoras.cards.shared.protocol.chat.{ChatUser, OutputMessage}
import io.tyoras.cards.util.validation.string.{BlankFieldError, TooLongError, TooShortError}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID

class OutputMessageSpec extends AnyFlatSpec with Matchers with EitherValues with SharedCodecTestUtils with FlatSpecDiscipline with Configuration:

  private val alice = new ChatUser("alice")
  private val bob   = new ChatUser("bob", Some(User.ID(UUID.randomUUID())))

  "ChatUser.apply" should "build a user without id from a valid name" in {
    ChatUser("alice").toEither.value shouldBe new ChatUser("alice", None)
  }

  it should "reject a blank name" in {
    ChatUser("   ").toEither.left.value.toList shouldBe List(BlankFieldError("Chat user name"))
  }

  it should "reject a name that is too short" in {
    ChatUser("a").toEither.left.value.toList shouldBe List(TooShortError("Chat user name", 2))
  }

  it should "reject a name that is too long" in {
    ChatUser("a" * 11).toEither.left.value.toList shouldBe List(TooLongError("Chat user name", 10))
  }

  it should "accumulate errors" in {
    ChatUser(" ").toEither.left.value.toList shouldBe List(BlankFieldError("Chat user name"), TooShortError("Chat user name", 2))
  }

  it should "accept names at the length boundaries" in {
    ChatUser("ab").isValid shouldBe true
    ChatUser("a" * 10).isValid shouldBe true
  }

  "OutputMessage.message" should "describe how to register" in {
    OutputMessage.Register.message should include("/name <username>")
  }

  it should "expose the wrapped message" in {
    ParsingError("bad input").message shouldBe "bad input"
    AuthError("denied").message shouldBe "denied"
    SendToUser(alice, "hi").message shouldBe "hi"
    ChatMsg(alice, bob, "hello").message shouldBe "hello"
  }

  it should "announce a successful registration" in {
    SuccessfulRegistration(alice).message shouldBe "alice entered the chat"
  }

  it should "use fixed messages for technical cases" in {
    UnsupportedCommand.message shouldBe "Unsupported command"
    KeepAlive.message shouldBe ""
    DiscardMessage.message shouldBe ""
  }

  "SendToUser.forUser" should "match only the recipient" in {
    SendToUser(alice, "hi").forUser(alice) shouldBe true
    SendToUser(alice, "hi").forUser(bob) shouldBe false
  }

  "ChatMsg.forUser" should "match only the recipient" in {
    ChatMsg(alice, bob, "hi").forUser(bob) shouldBe true
    ChatMsg(alice, bob, "hi").forUser(alice) shouldBe false
  }

  "OutputMessage codec" should "add a message_type discriminator" in {
    (AuthError("denied"): OutputMessage).asJson.hcursor.get[String]("message_type").value shouldBe "AuthError"
  }

  it should "round-trip every message type" in {
    val messages: List[OutputMessage] = List(
      Register,
      ParsingError("bad"),
      AuthError("denied"),
      SuccessfulRegistration(bob),
      UnsupportedCommand,
      KeepAlive,
      DiscardMessage,
      SendToUser(alice, "hi"),
      ChatMsg(alice, bob, "hello")
    )

    messages.foreach(m => m.asJson.as[OutputMessage].value shouldBe m)
  }

  "ChatUser codec" should "round-trip through JSON" in {
    bob.asJson.as[ChatUser].value shouldBe bob
  }

  // golden JSON codec tests
  checkAll("GoldenCodec[ChatUser]", GoldenCodecTests[ChatUser].goldenCodec)
  checkAll("GoldenCodec[OutputMessage]", GoldenCodecTests[OutputMessage].goldenCodec)
  // every case of the enum
  checkAll("GoldenCodec[AllChatOutputMessages]", GoldenCodecTests[AllChatOutputMessages].goldenCodec)
