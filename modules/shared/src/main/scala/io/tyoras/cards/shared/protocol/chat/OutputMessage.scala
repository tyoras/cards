package io.tyoras.cards.shared.protocol.chat

import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.util.validation.string.*
import io.tyoras.cards.util.validation.ValidationResult
import io.tyoras.cards.util.validation.syntax.*
import io.chrisdavenport.fuuid.circe.*
import io.circe.derivation.{ConfiguredDecoder, ConfiguredEncoder}
import io.circe.{Codec, Decoder, Encoder}

final case class ChatUser(name: String, id: Option[FUUID] = None) derives Codec
object ChatUser:
  def apply(name: String): ValidationResult[ChatUser] =
    name.optionalWithDefault("Chat user name", notBlank, min(2), max(10)).map(new ChatUser(_))

enum OutputMessage(val message: String):
  case Register extends OutputMessage("""|Register your username with the following command:
                                         |/name <username>""".stripMargin)
  case ParsingError(msg: String)                          extends OutputMessage(msg)
  case AuthError(msg: String)                             extends OutputMessage(msg)
  case SuccessfulRegistration(user: ChatUser)             extends OutputMessage(s"${user.name} entered the chat")
  case UnsupportedCommand                                 extends OutputMessage("Unsupported command")
  case KeepAlive                                          extends OutputMessage("")
  case DiscardMessage                                     extends OutputMessage("")
  case SendToUser(user: ChatUser, msg: String)            extends OutputMessage(msg)
  case ChatMsg(from: ChatUser, to: ChatUser, msg: String) extends OutputMessage(msg)

object OutputMessage:
  given Encoder[OutputMessage] = ConfiguredEncoder.derive(discriminator = Some("message_type"))
  given Decoder[OutputMessage] = ConfiguredDecoder.derive(discriminator = Some("message_type"))

  extension (m: SendToUser) def forUser(targetUser: ChatUser): Boolean = targetUser == m.user
  extension (m: ChatMsg) def forUser(targetUser: ChatUser): Boolean    = targetUser == m.to
