package io.tyoras.cards.server.protocol.chat

import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import io.tyoras.cards.server.endpoints.utils.{TestAuthService, TestData}
import io.tyoras.cards.shared.protocol.chat.{ChatUser, OutputMessage}
import io.tyoras.cards.shared.protocol.chat.OutputMessage.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

class InputParserSpec extends AnyFunSpec with Matchers:
  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val alice = ChatUser("alice", None)

  /** Parses the inputs in sequence with the given connected user and returns the output of the last input with the final connected user */
  private def parse(initialUser: Option[ChatUser], inputs: String*): (List[OutputMessage], Option[ChatUser]) =
    (for
      protocol <- ChatProtocol.make[IO](new TestAuthService)
      parser = InputParser.make[IO](protocol)
      _       <- initialUser.fold(IO.unit)(u => protocol.enterRoom(u, Room.defaultRoom).void)
      userRef <- Ref.of[IO, Option[ChatUser]](initialUser)
      outputs <- inputs.toList.traverse(parser.parse(userRef, _))
      user    <- userRef.get
    yield outputs.last -> user).unsafeRunSync()

  describe("any user") {
    it("should discard blank inputs") {
      parse(None, "   ")._1 shouldBe List(DiscardMessage)
      parse(Some(alice), "")._1 shouldBe List(DiscardMessage)
    }
    it("should reject commands with non alphabetic characters") {
      parse(None, "/n4me bob")._1 shouldBe List(ParsingError("Characters after '/' must be between A-Z or a-z"))
      parse(Some(alice), "/room!")._1 shouldBe List(ParsingError("Characters after '/' must be between A-Z or a-z"))
    }
  }

  describe("unregistered user") {
    it("should be asked to register when sending plain text") {
      parse(None, "hello") shouldBe (List(Register) -> None)
    }
    it("should register with the /name command and enter the default room") {
      val (messages, user) = parse(None, "/name bob")
      val bob              = ChatUser("bob", None)
      user shouldBe Some(bob)
      messages shouldBe List(SendToUser(bob, "/help shows all available commands"), SendToUser(bob, "bob has joined the room"))
    }
    it("should not register with a name already in use") {
      parse(None, "/name alice", "/name alice") match
        case (messages, user) =>
          messages shouldBe List(ParsingError("You can't register again"))
          user shouldBe Some(alice)
    }
    it("should not register with an invalid name") {
      val (messages, user) = parse(None, "/name a_way_too_long_name")
      messages.loneElement shouldBe a[ParsingError]
      user shouldBe None
    }
    it("should register with the /auth command and a valid token") {
      val (messages, user) = parse(None, s"/auth ${TestData.existingUserAuthToken.value}")
      user.map(_.name) shouldBe Some(TestData.existingUser.name: String)
      messages.head shouldBe SendToUser(user.get, "/help shows all available commands")
    }
    it("should not register with the /auth command and an invalid token") {
      parse(None, "/auth invalid") shouldBe (List(AuthError("Invalid token: unknown token")) -> None)
    }
    it("should not support other commands") {
      parse(None, "/rooms") shouldBe (List(UnsupportedCommand) -> None)
      parse(None, "/name") shouldBe (List(UnsupportedCommand)  -> None)
    }
  }

  describe("registered user") {
    it("should chat with plain text") {
      parse(Some(alice), "hello")._1 shouldBe List(ChatMsg(alice, alice, "hello"))
    }
    it("should not register again") {
      parse(Some(alice), "/name other")._1 shouldBe List(ParsingError("You can't register again"))
    }
    it("should not authenticate again") {
      parse(Some(alice), "/auth token")._1 shouldBe List(ParsingError("You are already authenticated"))
    }
    it("should change room") {
      parse(Some(alice), "/room lobby")._1 should contain(SendToUser(alice, "alice has joined the room"))
    }
    it("should reject an invalid room name") {
      parse(Some(alice), "/room x")._1.loneElement shouldBe a[ParsingError]
    }
    it("should get the help") {
      parse(Some(alice), "/help")._1.loneElement shouldBe a[SendToUser]
    }
    it("should list the rooms") {
      parse(Some(alice), "/rooms")._1 shouldBe List(SendToUser(alice, "Rooms:\n  Default"))
    }
    it("should list the room members") {
      parse(Some(alice), "/members")._1 shouldBe List(SendToUser(alice, "Room Members:\n  alice"))
    }
    it("should not support unknown commands") {
      parse(Some(alice), "/unknown")._1 shouldBe List(UnsupportedCommand)
      parse(Some(alice), "/rooms extra")._1 shouldBe List(UnsupportedCommand)
    }
  }

  extension [A](l: List[A])
    private def loneElement: A =
      l should have size 1
      l.head
