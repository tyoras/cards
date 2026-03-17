package io.tyoras.cards.server.protocol.chat

import cats.effect.{Ref, Sync}
import cats.syntax.all.*
import cats.data.Validated.*
import dev.profunktor.auth.jwt.JwtToken
import io.tyoras.cards.domain.auth.{AuthError, AuthService}
import OutputMessage.{ChatMsg, DiscardMessage, ParsingError, SendToUser, SuccessfulRegistration}
import org.typelevel.log4cats.LoggerFactory

trait ChatProtocol[F[_]]:
  def currentState: F[ChatState]
  def register(name: String): F[OutputMessage]
  def auth(jwt: JwtToken): F[OutputMessage]
  def isUsernameInUse(name: String): F[Boolean]
  def enterRoom(user: ChatUser, room: Room): F[List[OutputMessage]]
  def chat(user: ChatUser, text: String): F[List[OutputMessage]]
  def help(user: ChatUser): F[OutputMessage]
  def listRooms(user: ChatUser): F[List[OutputMessage]]
  def listMembers(user: ChatUser): F[List[OutputMessage]]
  def disconnect(userRef: Ref[F, Option[ChatUser]]): F[List[OutputMessage]]

object ChatProtocol:
  def make[F[_] : Sync : LoggerFactory](authService: AuthService[F]): F[ChatProtocol[F]] =
    Ref.of(ChatState.empty).map { chatState =>
      new ChatProtocol[F]:
        private val logger                      = LoggerFactory.getLogger
        override def currentState: F[ChatState] = chatState.get

        override def register(name: String): F[OutputMessage] =
          ChatUser(name) match
            case Valid(u)   => SuccessfulRegistration(u).pure
            case Invalid(e) => ParsingError(e.toString).pure

        override def auth(jwt: JwtToken): F[OutputMessage] =
          authService.authenticate(jwt).flatMap(user => register(user.name)).handleError {
            case e: AuthError => OutputMessage.AuthError(e.message)
            case _            => OutputMessage.AuthError("unexpected auth error")
          }

        override def isUsernameInUse(name: String): F[Boolean] =
          chatState.get.map(_.roomsByUser.keySet.exists(_.name == name))

        override def enterRoom(user: ChatUser, room: Room): F[List[OutputMessage]] =
          chatState.get.flatMap {
            _.roomsByUser.get(user) match
              case Some(r) if r == room =>
                List(SendToUser(user, s"You are already in the ${room.room} room")).pure
              case Some(_) =>
                val leaveMessages = removeFromCurrentRoom(chatState, user)
                val enterMessages = addToRoom(chatState, user, room)
                for
                  leave <- leaveMessages
                  enter <- enterMessages
                yield leave ++ enter
              case None => addToRoom(chatState, user, room)
          }

        private def addToRoom(stateRef: Ref[F, ChatState], user: ChatUser, room: Room): F[List[OutputMessage]] =
          stateRef
            .updateAndGet { cs =>
              val updatedMemberList = cs.roomMembers.getOrElse(room, Set()) + user
              ChatState(cs.roomsByUser + (user -> room), cs.roomMembers + (room -> updatedMemberList))
            }
            .flatMap {
              broadcastMessage(_, room, SendToUser(user, s"${user.name} has joined the room"))
            }

        private def removeFromCurrentRoom(stateRef: Ref[F, ChatState], user: ChatUser): F[List[OutputMessage]] =
          stateRef.get.flatMap { cs =>
            cs.roomsByUser.get(user) match
              case Some(room) =>
                val updateMembers = cs.roomMembers.getOrElse(room, Set.empty) - user
                stateRef.update { ccs =>
                  ChatState(
                    ccs.roomsByUser - user,
                    if updateMembers.isEmpty then ccs.roomMembers - room
                    else ccs.roomMembers + (room -> updateMembers)
                  )
                } >> broadcastMessage(cs, room, SendToUser(user, s"${user.name} has left the ${room.room} room"))
              case None =>
                List.empty[OutputMessage].pure[F]
          }

        override def chat(user: ChatUser, text: String): F[List[OutputMessage]] =
          for
            cs <- chatState.get
            output <- cs.roomsByUser.get(user) match
              case Some(room) => broadcastMessage(cs, room, ChatMsg(user, user, text))
              case None       => List(SendToUser(user, "You are not currently in a room")).pure[F]
          yield output

        private def broadcastMessage(cs: ChatState, room: Room, om: OutputMessage): F[List[OutputMessage]] =
          cs.roomMembers
            .getOrElse(room, Set.empty)
            .map { user =>
              om match
                case SendToUser(u, msg)     => SendToUser(user, msg)
                case ChatMsg(from, to, msg) => ChatMsg(from, user, msg)
                case _                      => DiscardMessage
            }
            .toList
            .pure

        override def help(user: ChatUser): F[OutputMessage] =
          val text =
            """Commands:
              | /help             - Show this text
              | /room             - Change to default/entry room
              | /room <room name> - Change to specified room
              | /rooms            - List all rooms
              | /members          - List members in current room
                """.stripMargin
          SendToUser(user, text).pure

        override def listRooms(user: ChatUser): F[List[OutputMessage]] =
          chatState.get.map { cs =>
            val roomList =
              cs.roomMembers.keys.map(_.room).toList.sorted.mkString("Rooms:\n\t", "\n\t", "")
            List(SendToUser(user, roomList))
          }

        override def listMembers(user: ChatUser): F[List[OutputMessage]] =
          chatState.get.map { cs =>
            val memberList =
              cs.roomsByUser.get(user) match
                case Some(room) =>
                  cs.roomMembers.getOrElse(room, Set.empty).map(_.name).toList.sorted.mkString("Room Members:\n\t", "\n\t", "")
                case None => "You are not currently in a room"
            List(SendToUser(user, memberList))
          }

        override def disconnect(userRef: Ref[F, Option[ChatUser]]): F[List[OutputMessage]] =
          userRef.get.flatMap:
            case Some(user) => logger.info(s"Disconnecting user $user from chat") *> removeFromCurrentRoom(chatState, user)
            case None       => List.empty.pure
    }
