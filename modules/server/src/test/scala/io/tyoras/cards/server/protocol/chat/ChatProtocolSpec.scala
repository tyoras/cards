package io.tyoras.cards.server.protocol.chat

import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import dev.profunktor.auth.jwt.JwtToken
import io.tyoras.cards.server.endpoints.utils.{TestAuthService, TestData}
import io.tyoras.cards.shared.protocol.chat.ChatUser
import io.tyoras.cards.shared.protocol.chat.OutputMessage.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

class ChatProtocolSpec extends AnyFunSpec with Matchers:
  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val alice = ChatUser("alice", None)
  private val bob   = ChatUser("bob", None)
  private val lobby = Room("lobby").toOption.get

  private def withProtocol[A](test: ChatProtocol[IO] => IO[A]): A =
    ChatProtocol.make[IO](new TestAuthService).flatMap(test).unsafeRunSync()

  describe("register") {
    it("should succeed with a valid name") {
      withProtocol(_.register("alice")) shouldBe SuccessfulRegistration(alice)
    }
    it("should fail with a too long name") {
      withProtocol(_.register("a_way_too_long_name")) shouldBe a[ParsingError]
    }
    it("should fail with a blank name") {
      withProtocol(_.register("  ")) shouldBe a[ParsingError]
    }
  }

  describe("auth") {
    it("should register the user owning a valid token") {
      withProtocol(_.auth(TestData.existingUserAuthToken)) shouldBe SuccessfulRegistration(ChatUser(TestData.existingUser.name, None))
    }
    it("should return an auth error with an invalid token") {
      withProtocol(_.auth(JwtToken("invalid"))) shouldBe AuthError("Invalid token: unknown token")
    }
  }

  describe("enterRoom") {
    it("should add the user to the room and notify the room members") {
      val (messages, state) = withProtocol { p =>
        for
          _     <- p.enterRoom(alice, Room.defaultRoom)
          msgs  <- p.enterRoom(bob, Room.defaultRoom)
          state <- p.currentState
        yield (msgs, state)
      }
      messages should contain theSameElementsAs List(SendToUser(alice, "bob has joined the room"), SendToUser(bob, "bob has joined the room"))
      state.roomsByUser shouldBe Map(alice -> Room.defaultRoom, bob -> Room.defaultRoom)
      state.roomMembers shouldBe Map(Room.defaultRoom -> Set(alice, bob))
    }

    it("should tell the user when he is already in the room") {
      val messages = withProtocol(p => p.enterRoom(alice, lobby) >> p.enterRoom(alice, lobby))
      messages shouldBe List(SendToUser(alice, "You are already in the lobby room"))
    }

    it("should move the user from his current room to the new one") {
      val (messages, state) = withProtocol { p =>
        for
          _     <- p.enterRoom(alice, Room.defaultRoom)
          _     <- p.enterRoom(bob, Room.defaultRoom)
          msgs  <- p.enterRoom(alice, lobby)
          state <- p.currentState
        yield (msgs, state)
      }
      (messages should contain).allOf(
        SendToUser(bob, "alice has left the Default room"),
        SendToUser(alice, "alice has joined the room")
      )
      state.roomsByUser shouldBe Map(alice -> lobby, bob -> Room.defaultRoom)
      state.roomMembers shouldBe Map(Room.defaultRoom -> Set(bob), lobby -> Set(alice))
    }

    it("should remove the room when its last member leaves") {
      val state = withProtocol(p => p.enterRoom(alice, Room.defaultRoom) >> p.enterRoom(alice, lobby) >> p.currentState)
      state.roomMembers shouldBe Map(lobby -> Set(alice))
    }
  }

  describe("isUsernameInUse") {
    it("should only be true for users present in a room") {
      val (inUse, notInUse) = withProtocol(p => p.enterRoom(alice, lobby) >> p.isUsernameInUse("alice").both(p.isUsernameInUse("bob")))
      inUse shouldBe true
      notInUse shouldBe false
    }
  }

  describe("chat") {
    it("should broadcast the message to all the room members") {
      val messages = withProtocol(p => p.enterRoom(alice, lobby) >> p.enterRoom(bob, lobby) >> p.chat(alice, "hello"))
      messages should contain theSameElementsAs List(ChatMsg(alice, alice, "hello"), ChatMsg(alice, bob, "hello"))
    }
    it("should not broadcast to members of other rooms") {
      val messages = withProtocol(p => p.enterRoom(alice, lobby) >> p.enterRoom(bob, Room.defaultRoom) >> p.chat(alice, "hello"))
      messages shouldBe List(ChatMsg(alice, alice, "hello"))
    }
    it("should warn a user who is not in a room") {
      withProtocol(_.chat(alice, "hello")) shouldBe List(SendToUser(alice, "You are not currently in a room"))
    }
  }

  describe("help") {
    it("should list the available commands") {
      val SendToUser(user, text) = withProtocol(_.help(alice)).asInstanceOf[SendToUser]
      user shouldBe alice
      List("/help", "/room", "/rooms", "/members").foreach(cmd => text should include(cmd))
    }
  }

  describe("listRooms") {
    it("should list the rooms sorted by name") {
      val messages = withProtocol(p => p.enterRoom(alice, lobby) >> p.enterRoom(bob, Room.defaultRoom) >> p.listRooms(alice))
      messages shouldBe List(SendToUser(alice, "Rooms:\n  Default\n  lobby"))
    }
  }

  describe("listMembers") {
    it("should list the members of the user's room") {
      val List(SendToUser(user, text)) = withProtocol(p => p.enterRoom(alice, lobby) >> p.enterRoom(bob, lobby) >> p.listMembers(alice)): @unchecked
      user shouldBe alice
      text should startWith("Room Members:\n  alice")
      text should include("bob")
    }
    it("should warn a user who is not in a room") {
      withProtocol(_.listMembers(alice)) shouldBe List(SendToUser(alice, "You are not currently in a room"))
    }
  }

  describe("disconnect") {
    it("should remove the user from his room and notify the other members") {
      val (messages, state) = withProtocol { p =>
        for
          _       <- p.enterRoom(alice, lobby)
          _       <- p.enterRoom(bob, lobby)
          userRef <- Ref.of[IO, Option[ChatUser]](Some(alice))
          msgs    <- p.disconnect(userRef)
          state   <- p.currentState
        yield (msgs, state)
      }
      messages should contain(SendToUser(bob, "alice has left the lobby room"))
      state.roomsByUser shouldBe Map(bob -> lobby)
    }
    it("should do nothing when no user is connected") {
      withProtocol(p => Ref.of[IO, Option[ChatUser]](None).flatMap(p.disconnect)) shouldBe empty
    }
  }
