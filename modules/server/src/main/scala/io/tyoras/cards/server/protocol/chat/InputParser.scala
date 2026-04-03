package io.tyoras.cards.server.protocol.chat

import cats.Monad
import cats.effect.Ref
import cats.syntax.all.*
import cats.parse.{Parser, Rfc5234}
import cats.parse.Parser.char
import cats.parse.Rfc5234.{alpha, sp, wsp}
import cats.data.Validated.*
import dev.profunktor.auth.jwt.JwtToken
import io.tyoras.cards.shared.protocol.chat.OutputMessage.*
import io.tyoras.cards.shared.protocol.chat.*

trait InputParser[F[_]]:
  def parse(userRef: Ref[F, Option[ChatUser]], text: String): F[List[OutputMessage]]

object InputParser:
  private case class TextCommand(left: String, right: Option[String])

  def make[F[_] : Monad](protocol: ChatProtocol[F]): InputParser[F] =
    new InputParser[F]:
      override def parse(userRef: Ref[F, Option[ChatUser]], text: String): F[List[OutputMessage]] =
        text.trim match
          case "" => List(DiscardMessage).pure
          case txt =>
            userRef.get.flatMap {
              _.fold(processText4UnReg(txt, userRef, Room.defaultRoom)) { user =>
                processText4Reg(user, txt)
              }
            }

      private def commandParser: Parser[TextCommand] =
        val leftSide                          = (char('/').string ~ alpha.rep.string).string
        val rightSide: Parser[(Unit, String)] = sp ~ Rfc5234.char.rep.string
        ((leftSide ~ rightSide.?) <* wsp.rep.?).map((left, right) => TextCommand(left, right.map(_._2)))

      private def parseToTextCommand(value: String): Either[Parser.Error, TextCommand] =
        commandParser.parseAll(value)

      private def processText4UnReg(text: String, userRef: Ref[F, Option[ChatUser]], room: Room): F[List[OutputMessage]] =
        if text.charAt(0) == '/' then
          parseToTextCommand(text).fold(
            _ => List(ParsingError("Characters after '/' must be between A-Z or a-z")).pure,
            {
              case TextCommand("/name", Some(n)) =>
                protocol.isUsernameInUse(n).flatMap { b =>
                  if b then List(ParsingError("User name already in use")).pure
                  else protocol.register(n).flatMap(handleRegistration(_, userRef, room))
                }
              case TextCommand("/auth", Some(rawJWT)) =>
                protocol.auth(JwtToken(rawJWT)).flatMap(handleRegistration(_, userRef, room))
              case _ => List(UnsupportedCommand).pure
            }
          )
        else List(Register).pure

      private def handleRegistration(outputMessage: OutputMessage, userRef: Ref[F, Option[ChatUser]], room: Room): F[List[OutputMessage]] =
        outputMessage match
          case SuccessfulRegistration(u) =>
            for
              _  <- userRef.set(Some(u))
              om <- protocol.enterRoom(u, room)
            yield List(SendToUser(u, "/help shows all available commands")) ++ om
          case e: ParsingError => List(e).pure
          case e: AuthError    => List(e).pure
          case _               => List.empty.pure

      private def processText4Reg(user: ChatUser, text: String): F[List[OutputMessage]] =
        if text.charAt(0) == '/' then
          parseToTextCommand(text).fold(
            _ => List(ParsingError("Characters after '/' must be between A-Z or a-z")).pure,
            {
              case TextCommand("/auth", Some(_)) => List(ParsingError("You are already authenticated")).pure
              case TextCommand("/name", Some(n)) => List(ParsingError("You can't register again")).pure
              case TextCommand("/room", Some(r)) =>
                Room(r) match
                  case Valid(room) => protocol.enterRoom(user, room)
                  case Invalid(e)  => List(ParsingError(e.map(field => s"$field.code | ${field.message}").mkString_(","))).pure
              case TextCommand("/help", None)    => protocol.help(user).map(List(_))
              case TextCommand("/rooms", None)   => protocol.listRooms(user)
              case TextCommand("/members", None) => protocol.listMembers(user)
              case _                             => List(UnsupportedCommand).pure
            }
          )
        else protocol.chat(user, text)
