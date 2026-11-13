package io.tyoras.cards.tests

import cats.Eq
import cats.data.NonEmptyList
import dev.profunktor.auth.jwt.JwtToken
import io.circe.{Decoder, Encoder, Json}
import io.tyoras.cards.domain.game.GameType
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.endpoint.ErrorPayloads.Response.{ApiFieldError, ApiMessage}
import io.tyoras.cards.shared.endpoint.admin.health.Payloads.Response.HealthStatus
import io.tyoras.cards.shared.endpoint.auth.Payloads as AuthPayloads
import io.tyoras.cards.shared.endpoint.games.Payloads as GamePayloads
import io.tyoras.cards.shared.endpoint.games.ers.Payloads as ErsPayloads
import io.tyoras.cards.shared.endpoint.games.war.Payloads as WarPayloads
import io.tyoras.cards.shared.endpoint.users.Payloads as UserPayloads
import io.tyoras.cards.shared.protocol.chat.{ChatUser, OutputMessage as ChatOutputMessage}
import io.tyoras.cards.shared.protocol.game.Commands.{AuthCommand, AuthenticatedCommand}
import io.tyoras.cards.shared.protocol.game.OutputMessage as GameOutputMessage
import org.scalacheck.{Arbitrary, Gen}

import java.time.{Instant, ZoneOffset, ZonedDateTime}

/** Deterministic generators (seed driven) for the shared payloads, used by the circe golden codec tests. */
object SharedGens:
  val textGen: Gen[String]                   = Gen.choose(1, 20).flatMap(Gen.listOfN(_, Gen.alphaNumChar)).map(_.mkString)
  val optTextGen: Gen[Option[String]]        = Gen.option(textGen)
  val gameTypeGen: Gen[GameType]             = Gen.oneOf(io.tyoras.cards.domain.game.GameTyp.Schnapsen, io.tyoras.cards.domain.game.GameTyp.War)
  val countGen: Gen[Int]                     = Gen.choose(0, 1000)
  val playersGen: Gen[NonEmptyList[User.ID]] =
    Gen.choose(1, 4).flatMap(n => Gen.listOfN(n, userIdGen)).map(l => NonEmptyList.fromListUnsafe(l))

  // whole seconds in UTC so that the ISO representation round-trips to an equal value
  val dateTimeGen: Gen[ZonedDateTime] =
    Gen.choose(0L, 4102444800L).map(s => ZonedDateTime.ofInstant(Instant.ofEpochSecond(s), ZoneOffset.UTC))

  val jsonGen: Gen[Json] = Gen.oneOf(
    Gen.const(Json.Null),
    Gen.oneOf(true, false).map(Json.fromBoolean),
    Gen.choose(-1000, 1000).map(Json.fromInt),
    textGen.map(Json.fromString),
    Gen.listOfN(2, Gen.zip(textGen, textGen)).map(kvs => Json.obj(kvs.map((k, v) => k -> Json.fromString(v))*))
  )

  val apiFieldErrorGen: Gen[ApiFieldError] = Gen.resultOf(ApiFieldError.apply)
  val apiMessageGen: Gen[ApiMessage]       =
    for
      code    <- textGen
      message <- textGen
      errors  <- Gen.choose(0, 3).flatMap(Gen.listOfN(_, apiFieldErrorGen))
    yield ApiMessage(code, message, errors)

  val statusGen: Gen[String]                         = Gen.oneOf("UP", "DOWN")
  def healthStatusGen(depth: Int): Gen[HealthStatus] =
    for
      name       <- textGen
      status     <- statusGen
      components <- if depth <= 0 then Gen.const(Nil) else Gen.choose(0, 2).flatMap(Gen.listOfN(_, healthStatusGen(depth - 1)))
    yield HealthStatus(name, status, components)

  val chatUserGen: Gen[ChatUser] = Gen.zip(textGen, Gen.option(userIdGen)).map((name, id) => new ChatUser(name, id))

  val chatOutputMessageCaseGens: List[Gen[ChatOutputMessage]] =
    import ChatOutputMessage.*
    List(
      Gen.const(Register),
      textGen.map(ParsingError(_)),
      textGen.map(AuthError(_)),
      chatUserGen.map(SuccessfulRegistration(_)),
      Gen.const(UnsupportedCommand),
      Gen.const(KeepAlive),
      Gen.const(DiscardMessage),
      Gen.zip(chatUserGen, textGen).map(SendToUser(_, _)),
      Gen.zip(chatUserGen, chatUserGen, textGen).map(ChatMsg(_, _, _))
    )

  val authenticatedCommandCaseGens: List[Gen[AuthenticatedCommand]] =
    import AuthenticatedCommand.*
    List(
      gameIdGen.map(QuitCommand(_)),
      gameIdGen.map(StateCommand(_)),
      Gen.zip(gameIdGen, jsonGen).map(GameCommand(_, _))
    )

  val gameOutputMessageCaseGens: List[Gen[GameOutputMessage]] =
    import GameOutputMessage.*
    List(
      Gen.const(KeepAlive),
      Gen.const(DiscardMessage),
      Gen.zip(textGen, textGen).map(AuthError(_, _)),
      Gen.const(UnsupportedCommand),
      Gen.zip(gameIdGen, gameTypeGen, userIdGen, textGen).map(PlayerConnectionSuccess(_, _, _, _)),
      Gen.zip(gameIdGen, gameTypeGen, userIdGen, textGen).map(PlayerDisconnected(_, _, _, _)),
      Gen.zip(gameIdGen, userIdGen, jsonGen).map(GameState(_, _, _)),
      Gen.zip(gameIdGen, userIdGen, textGen, textGen).map(GameError(_, _, _, _)),
      Gen.zip(gameIdGen, userIdGen, textGen, textGen).map(ProtocolError(_, _, _, _)),
      gameIdGen.map(GameEnded(_))
    )

  private def oneOfCases[A](cases: List[Gen[A]]): Gen[A] = Gen.choose(0, cases.size - 1).flatMap(cases(_))

  val chatOutputMessageGen: Gen[ChatOutputMessage]       = oneOfCases(chatOutputMessageCaseGens)
  val authenticatedCommandGen: Gen[AuthenticatedCommand] = oneOfCases(authenticatedCommandCaseGens)
  val gameOutputMessageGen: Gen[GameOutputMessage]       = oneOfCases(gameOutputMessageCaseGens)

trait SharedCodecTestUtils extends CodecTestUtils:
  import SharedGens.*

  private def universalEq[A]: Eq[A] = Eq.fromUniversalEquals

  // error payloads
  given Arbitrary[ApiMessage] = Arbitrary(apiMessageGen)
  given Eq[ApiMessage]        = universalEq

  // health
  given Arbitrary[HealthStatus] = Arbitrary(healthStatusGen(2))
  given Eq[HealthStatus]        = universalEq

  // auth
  given Arbitrary[AuthPayloads.Request.Login]            = Arbitrary(Gen.zip(optTextGen, optTextGen).map(AuthPayloads.Request.Login(_, _)))
  given Eq[AuthPayloads.Request.Login]                   = universalEq
  given Arbitrary[AuthPayloads.Response.SuccessfulLogin] =
    Arbitrary(Gen.zip(textGen.map(JwtToken(_)), userIdGen).map(AuthPayloads.Response.SuccessfulLogin(_, _)))
  given Eq[AuthPayloads.Response.SuccessfulLogin] = universalEq

  // games
  given arbGameCreation: Arbitrary[GamePayloads.Request.Creation] = Arbitrary(Gen.listOf(Gen.uuid).map(GamePayloads.Request.Creation(_)))
  given eqGameCreation: Eq[GamePayloads.Request.Creation]         = universalEq
  given Arbitrary[GamePayloads.Response.Game]                     =
    Arbitrary(Gen.zip(gameIdGen, dateTimeGen, dateTimeGen, gameTypeGen, playersGen).map(GamePayloads.Response.Game(_, _, _, _, _)))
  given Eq[GamePayloads.Response.Game] = universalEq

  // war
  given arbWarCreation: Arbitrary[WarPayloads.Request.Creation] = Arbitrary(playersGen.map(WarPayloads.Request.Creation(_)))
  given eqWarCreation: Eq[WarPayloads.Request.Creation]         = universalEq

  // egyptian ratscrew
  given arbErsCreation: Arbitrary[ErsPayloads.Request.Creation] = Arbitrary(playersGen.map(ErsPayloads.Request.Creation(_)))
  given eqErsCreation: Eq[ErsPayloads.Request.Creation]         = universalEq

  // users
  given arbUserCreation: Arbitrary[UserPayloads.Request.Creation] =
    Arbitrary(Gen.zip(optTextGen, optTextGen, optTextGen).map(UserPayloads.Request.Creation(_, _, _)))
  given eqUserCreation: Eq[UserPayloads.Request.Creation] = universalEq
  given Arbitrary[UserPayloads.Request.PasswordChange]    =
    Arbitrary(Gen.zip(optTextGen, optTextGen).map(UserPayloads.Request.PasswordChange(_, _)))
  given Eq[UserPayloads.Request.PasswordChange] = universalEq
  given Arbitrary[UserPayloads.Response.User]   =
    Arbitrary(Gen.zip(userIdGen, dateTimeGen, dateTimeGen, textGen, textGen).map(UserPayloads.Response.User(_, _, _, _, _)))
  given Eq[UserPayloads.Response.User]                 = universalEq
  given Arbitrary[UserPayloads.Response.UserGameStats] =
    Arbitrary(Gen.zip(gameTypeGen, countGen, countGen, countGen, dateTimeGen).map(UserPayloads.Response.UserGameStats(_, _, _, _, _)))
  given Eq[UserPayloads.Response.UserGameStats] = universalEq

  // chat protocol
  given Arbitrary[ChatUser]                                = Arbitrary(chatUserGen)
  given Eq[ChatUser]                                       = universalEq
  given arbChatOutputMessage: Arbitrary[ChatOutputMessage] = Arbitrary(chatOutputMessageGen)
  given eqChatOutputMessage: Eq[ChatOutputMessage]         = universalEq

  // game protocol
  given Arbitrary[AuthCommand]                             = Arbitrary(Gen.zip(gameIdGen, gameTypeGen, textGen).map(AuthCommand(_, _, _)))
  given Eq[AuthCommand]                                    = universalEq
  given Arbitrary[AuthenticatedCommand]                    = Arbitrary(authenticatedCommandGen)
  given Eq[AuthenticatedCommand]                           = universalEq
  given arbGameOutputMessage: Arbitrary[GameOutputMessage] = Arbitrary(gameOutputMessageGen)
  given eqGameOutputMessage: Eq[GameOutputMessage]         = universalEq

  // one value of every enum case, see AllVariants.scala
  given Arbitrary[AllChatOutputMessages] = Arbitrary(
    Gen.sequence[List[ChatOutputMessage], ChatOutputMessage](chatOutputMessageCaseGens).map(AllChatOutputMessages(_))
  )
  given Eq[AllChatOutputMessages]        = universalEq
  given Arbitrary[AllGameOutputMessages] = Arbitrary(
    Gen.sequence[List[GameOutputMessage], GameOutputMessage](gameOutputMessageCaseGens).map(AllGameOutputMessages(_))
  )
  given Eq[AllGameOutputMessages]           = universalEq
  given Arbitrary[AllAuthenticatedCommands] =
    Arbitrary(Gen.sequence[List[AuthenticatedCommand], AuthenticatedCommand](authenticatedCommandCaseGens).map(AllAuthenticatedCommands(_)))
  given Eq[AllAuthenticatedCommands] = universalEq

/* Golden files only freeze one generated value per file, which would leave most enum cases unchecked.
 * These wrappers are (de)serialized as a plain JSON array holding one value of every case,
 * so that a single golden file locks the format of all the cases of an enum. */
final case class AllChatOutputMessages(values: List[ChatOutputMessage])
object AllChatOutputMessages:
  given Encoder[AllChatOutputMessages] = Encoder.encodeList[ChatOutputMessage].contramap(_.values)
  given Decoder[AllChatOutputMessages] = Decoder.decodeList[ChatOutputMessage].map(AllChatOutputMessages(_))

final case class AllGameOutputMessages(values: List[GameOutputMessage])
object AllGameOutputMessages:
  given Encoder[AllGameOutputMessages] = Encoder.encodeList[GameOutputMessage].contramap(_.values)
  given Decoder[AllGameOutputMessages] = Decoder.decodeList[GameOutputMessage].map(AllGameOutputMessages(_))

final case class AllAuthenticatedCommands(values: List[AuthenticatedCommand])
object AllAuthenticatedCommands:
  given Encoder[AllAuthenticatedCommands] = Encoder.encodeList[AuthenticatedCommand].contramap(_.values)
  given Decoder[AllAuthenticatedCommands] = Decoder.decodeList[AuthenticatedCommand].map(AllAuthenticatedCommands(_))
