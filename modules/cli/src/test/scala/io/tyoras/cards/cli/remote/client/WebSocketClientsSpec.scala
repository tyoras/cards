package io.tyoras.cards.cli.remote.client

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Ref}
import io.circe.parser.decode
import io.circe.syntax.*
import io.tyoras.cards.cli.fixtures.{*, given}
import io.tyoras.cards.cli.remote.auth.AuthError.UserNotAuthenticated
import io.tyoras.cards.cli.remote.config.CardsClientConfig
import io.tyoras.cards.cli.remote.fakes.gameId
import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.card.Rank.Ace
import io.tyoras.cards.domain.card.Suit.Heart
import io.tyoras.cards.domain.game.GameTyp.War
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.domain.game.war.model.WarInput
import io.tyoras.cards.domain.game.war.model.WarInput.GameInput.{PlayCard, Ready}
import io.tyoras.cards.shared.protocol.chat.{ChatUser, OutputMessage as ChatMessage}
import io.tyoras.cards.shared.protocol.game.Commands.AuthenticatedCommand.{GameCommand, QuitCommand, StateCommand}
import io.tyoras.cards.shared.protocol.game.Commands.{AuthCommand, AuthenticatedCommand}
import io.tyoras.cards.shared.protocol.game.OutputMessage as GameMessage
import org.http4s.client.websocket.{WSFrame, WSRequest}
import org.http4s.implicits.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scodec.bits.ByteVector

import scala.concurrent.duration.DurationInt

class WebSocketClientsSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val config = CardsClientConfig(uri"http://cards.test", uri"ws://cards.test")
  private val card   = Card(Heart, Ace())

  private def text(json: io.circe.Json): WSFrame.Text = WSFrame.Text(json.noSpaces)

  private def commands(conn: FakeWsConnection): IO[List[AuthenticatedCommand]] =
    conn.sentTexts.map(_.drop(1).map(t => decode[AuthenticatedCommand](t).fold(throw _, identity)))

  "WarClient" should "authenticate to the game when created" in {
    for
      conn   <- FakeWsConnection.make(Nil)
      client <- WarClient.make[IO](gameId(1), conn, authProvider())
      sent   <- conn.sentTexts
    yield
      client.gameId shouldBe gameId(1)
      sent.map(decode[AuthCommand](_)) shouldBe List(Right(AuthCommand(gameId(1), War, token.value)))
  }

  it should "fail when the user is not authenticated" in
    FakeWsConnection.make(Nil).flatMap(conn => WarClient.make[IO](gameId(1), conn, unauthenticated).attempt).asserting(_ shouldBe Left(UserNotAuthenticated))

  it should "send the game inputs and commands" in {
    for
      conn   <- FakeWsConnection.make(Nil)
      client <- WarClient.make[IO](gameId(1), conn, authProvider())
      _      <- client.ready >> client.playCard(card) >> client.getState
      cmds   <- commands(conn)
    yield cmds shouldBe List(
      GameCommand(gameId(1), (Ready(alice.id): WarInput).asJson),
      GameCommand(gameId(1), (PlayCard(alice.id, card.id): WarInput).asJson),
      StateCommand(gameId(1))
    )
  }

  it should "decode the server messages, skipping the invalid ones" in {
    val messages = List[GameMessage](GameMessage.KeepAlive, GameMessage.GameEnded(gameId(1)))
    for
      conn     <- FakeWsConnection.make(List(text(messages.head.asJson), WSFrame.Text("not json"), WSFrame.Binary(ByteVector(1, 2)), text(messages(1).asJson)))
      client   <- WarClient.make[IO](gameId(1), conn, authProvider())
      received <- client.streamServerMessages.compile.toList.timeout(5.seconds)
    yield received shouldBe messages
  }

  it should "quit the game and stop streaming the server messages" in {
    for
      conn   <- FakeWsConnection.make(Nil, terminated = false)
      client <- WarClient.make[IO](gameId(1), conn, authProvider())
      fiber  <- client.streamServerMessages.compile.toList.start
      _      <- client.quit
      result <- fiber.joinWithNever.timeout(5.seconds)
      cmds   <- commands(conn)
    yield
      result shouldBe empty
      cmds shouldBe List(QuitCommand(gameId(1)))
  }

  "GamesClient" should "connect to the war game websocket" in {
    for
      conn     <- FakeWsConnection.make(Nil)
      requests <- Ref.of[IO, List[WSRequest]](Nil)
      games    <- IO(GamesClient.make[IO](config, org.http4s.client.Client[IO](_ => cats.effect.Resource.never), wsClient(conn, requests), authProvider()))
      id       <- games.connectWarGame(gameId(2)).use(c => IO.pure(c.gameId))
      reqs     <- requests.get
      sent     <- conn.sentTexts
    yield
      id shouldBe gameId(2)
      reqs.map(_.uri) shouldBe List(uri"ws://cards.test/ws/games/war")
      sent should have size 1
  }

  "ChatClient" should "authenticate to the chat when connecting" in {
    for
      conn     <- FakeWsConnection.make(Nil)
      requests <- Ref.of[IO, List[WSRequest]](Nil)
      _        <- ChatClient.make[IO](config, wsClient(conn, requests), authProvider()).connect.use_
      reqs     <- requests.get
      sent     <- conn.sentTexts
    yield
      reqs.map(_.uri) shouldBe List(uri"ws://cards.test/ws/chat")
      sent shouldBe List(s"/auth ${token.value}")
  }

  it should "send the chat commands and messages" in {
    for
      conn     <- FakeWsConnection.make(Nil)
      requests <- Ref.of[IO, List[WSRequest]](Nil)
      _        <- ChatClient.make[IO](config, wsClient(conn, requests), authProvider()).connect.use { chat =>
        chat.listRooms >> chat.changeRoom("lobby") >> chat.listRoomMembers >> chat.chat("hello")
      }
      sent <- conn.sentTexts
    yield sent.drop(1) shouldBe List("/rooms ", "/room lobby", "/members ", "hello")
  }

  it should "decode the server messages, skipping the invalid ones" in {
    val messages = List[ChatMessage](ChatMessage.KeepAlive, ChatMessage.ChatMsg(ChatUser("bob"), ChatUser("alice"), "hi"))
    for
      conn     <- FakeWsConnection.make(List(text(messages.head.asJson), WSFrame.Text("{}"), WSFrame.Binary(ByteVector(0)), text(messages(1).asJson)))
      requests <- Ref.of[IO, List[WSRequest]](Nil)
      received <- ChatClient.make[IO](config, wsClient(conn, requests), authProvider()).connect.use(_.streamServerMessages.compile.toList.timeout(5.seconds))
    yield received shouldBe messages
  }

  it should "stop streaming the server messages when disconnecting" in {
    for
      conn     <- FakeWsConnection.make(Nil, terminated = false)
      requests <- Ref.of[IO, List[WSRequest]](Nil)
      result   <- ChatClient.make[IO](config, wsClient(conn, requests), authProvider()).connect.use { chat =>
        chat.streamServerMessages.compile.toList.start.flatMap(fiber => chat.disconnect >> fiber.joinWithNever.timeout(5.seconds))
      }
    yield result shouldBe empty
  }

  it should "fail to connect when the user is not authenticated" in {
    for
      conn     <- FakeWsConnection.make(Nil)
      requests <- Ref.of[IO, List[WSRequest]](Nil)
      result   <- ChatClient.make[IO](config, wsClient(conn, requests), unauthenticated).connect.use_.attempt
    yield result shouldBe Left(UserNotAuthenticated)
  }
