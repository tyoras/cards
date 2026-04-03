package io.tyoras.cards.shared.protocol.chat

import dev.profunktor.auth.jwt.JwtToken
import cats.syntax.all.*

enum ChatCommand(cmd: String, arg: Option[String] = None):
  case Auth(jwt: JwtToken)          extends ChatCommand("auth", jwt.value.some)
  case ListRooms                    extends ChatCommand("rooms")
  case ListRoomMembers              extends ChatCommand("members")
  case ChangeRoom(roomName: String) extends ChatCommand("room", roomName.some)

  def asText: String = s"/$cmd ${arg.getOrElse("")}"
