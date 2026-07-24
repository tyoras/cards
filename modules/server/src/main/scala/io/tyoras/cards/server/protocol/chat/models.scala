package io.tyoras.cards.server.protocol.chat

import io.tyoras.cards.shared.protocol.chat.ChatUser
import io.tyoras.cards.util.validation.ValidationResult
import io.tyoras.cards.util.validation.string.*
import io.tyoras.cards.util.validation.syntax.*

final case class Room private (room: String)
object Room:
  val defaultRoom: Room                           = new Room("Default")
  def apply(name: String): ValidationResult[Room] =
    name.optionalWithDefault("Chat user name", notBlank, min(2), max(50)).map(new Room(_))

final case class ChatState(roomsByUser: Map[ChatUser, Room], roomMembers: Map[Room, Set[ChatUser]])
object ChatState:
  val empty: ChatState = ChatState(Map.empty, Map.empty)
