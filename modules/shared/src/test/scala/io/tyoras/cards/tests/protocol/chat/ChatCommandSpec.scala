package io.tyoras.cards.tests.protocol.chat

import dev.profunktor.auth.jwt.JwtToken
import io.tyoras.cards.shared.protocol.chat.ChatCommand
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChatCommandSpec extends AnyFlatSpec with Matchers:

  "ChatCommand.asText" should "render the auth command with the token" in {
    ChatCommand.Auth(JwtToken("a.b.c")).asText shouldBe "/auth a.b.c"
  }

  it should "render the list rooms command without argument" in {
    ChatCommand.ListRooms.asText shouldBe "/rooms "
  }

  it should "render the list members command without argument" in {
    ChatCommand.ListRoomMembers.asText shouldBe "/members "
  }

  it should "render the change room command with the room name" in {
    ChatCommand.ChangeRoom("lobby").asText shouldBe "/room lobby"
  }
