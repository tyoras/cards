package io.tyoras.cards.cli.remote.game.war

import cats.data.NonEmptyList
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.testkit.TestConsole
import cats.effect.{ExitCode, IO, Ref, Resource}
import io.circe.Json
import io.circe.syntax.*
import io.tyoras.cards.cli.fixtures.{*, given}
import io.tyoras.cards.cli.remote.client.{ChatClient, Game}
import io.tyoras.cards.cli.remote.fakes.*
import io.tyoras.cards.domain.card.Turn
import io.tyoras.cards.domain.game.Game.ID as GameID
import io.tyoras.cards.domain.game.GameTyp.War
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.domain.game.war.model.PlayerGameState
import io.tyoras.cards.shared.protocol.chat.{ChatUser, OutputMessage as ChatMessage}
import io.tyoras.cards.shared.protocol.game.OutputMessage as GameMessage
import layoutz.TestKit.ScriptedTerminal
import layoutz.TestKit.ScriptedTerminal.key as press
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class WarCliSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val ansi = "\u001b\\[[0-9;]*m".r

  private val warGame = game(1, War, NonEmptyList.of(alice.id, bob.id))
  private val id      = warGame.id

  private val initState: PlayerGameState = PlayerGameState.Init(Turn.applyUnsafe(1), alice.id, Set(bob.id), Set(alice.id))

  private val gameMessages = List(
    GameMessage.KeepAlive,
    GameMessage.PlayerConnectionSuccess(id, War, bob.id, "Bob"),
    GameMessage.GameState(id, alice.id, initState.asJson),
    GameMessage.GameError(id, alice.id, "bad_input", "Invalid input"),
    GameMessage.ProtocolError(id, alice.id, "bad_command", "Invalid command"),
    GameMessage.PlayerDisconnected(id, carol.id, "Carol"),
    GameMessage.GameEnded(id)
  )

  private val chatMessages = List(
    ChatMessage.SendToUser(ChatUser("alice"), "Welcome to the room"),
    ChatMessage.ChatMsg(ChatUser("bob"), ChatUser("alice"), "Good luck!"),
    ChatMessage.SuccessfulRegistration(ChatUser("dave")),
    ChatMessage.KeepAlive
  )

  // join the game from the start menu, then quit the game once it is displayed
  private def script = List(
    List(press('j'), press(ScriptedTerminal.Enter)),
    List(press(ScriptedTerminal.Escape, delayMs = 1500))
  )

  private def chatClient(connected: ChatClient.ConnectedClient[IO]): ChatClient[IO] = new:
    override def connect: Resource[IO, ChatClient.ConnectedClient[IO]] = Resource.pure(connected)

  private def runCli(
      gameMessages: List[GameMessage],
      findGame: GameID => IO[Option[Game]] = gid => IO.pure(Option.when(gid == id)(warGame)),
      hangOnDisconnect: Boolean = false
  ): IO[(ExitCode, List[String], String)] =
    for
      calls    <- Ref.of[IO, List[String]](Nil)
      created  <- Ref.of[IO, List[NonEmptyList[io.tyoras.cards.domain.game.war.model.PlayerId]]](Nil)
      terminal <- IO(ScriptedTerminal(script))
      warClient = new FakeWarClient(id, gameMessages, calls):
        override def quit: IO[Unit] = if hangOnDisconnect then IO.never else super.quit
      chat = new FakeChatClient(chatMessages, calls):
        override def disconnect: IO[Unit] = if hangOnDisconnect then IO.never else super.disconnect
      games = new FakeGamesClient(IO.pure(List(warGame)), created, Resource.pure(warClient)):
        override def findById(gameId: GameID): IO[Option[Game]] = findGame(gameId)
      cli = WarCli.WarCliImpl[IO](
        authProvider(),
        FakeUsersClient(IO.pure(List(alice, bob, carol))),
        games,
        chatClient(chat),
        Some(terminal)
      )
      exitCode <- cli.run.timeout(30.seconds)
      allCalls <- calls.get
    yield (exitCode, allCalls, ansi.replaceAllIn(terminal.output.synchronized(terminal.output.mkString), ""))

  "WarCli" should "be created from the configuration even if the login fails" in
    TestConsole
      .resource[IO]
      .use { (console, _, inspector) =>
        WarCli.make[IO](WarCli.Config(autoPlay = false))(using summon, summon, summon, console).product(inspector.stdErr)
      }
      .asserting { (cli, stdErr) =>
        cli shouldBe a[WarCli[IO]]
        stdErr shouldBe empty
      }

  it should "have a banner" in
    IO(WarCli.banner).asserting(_ should include("""\/  \/ \__,_|_|"""))

  "WarCli run" should "join a game, play it and disconnect gracefully" in {
    runCli(gameMessages).asserting { (exitCode, calls, output) =>
      exitCode shouldBe ExitCode.Success
      calls shouldBe List(s"room War game $id", "state", "quit", "disconnect")
      output should include("Waiting for everyone to be ready before starting the game...")
      output should (include("Player Bob has join the game")
        .and(include("Error bad_input : Invalid input"))
        .and(include("Error bad_command : Invalid command")))
      output should (include("Player Carol has disconnected from the game").and(include("Game has ended")))
      output should (include("Welcome to the room").and(include("Good luck!")).and(include("dave has Joined the chat room")))
    }
  }

  it should "give up disconnecting after a timeout" in {
    runCli(gameMessages, hangOnDisconnect = true).asserting { (exitCode, calls, _) =>
      exitCode shouldBe ExitCode.Success
      calls shouldBe List(s"room War game $id", "state")
    }
  }

  it should "survive an invalid game state from the server" in {
    val invalidState = List(GameMessage.GameState(id, alice.id, Json.obj("code" -> "unknown".asJson)))
    runCli(invalidState).asserting { (exitCode, calls, _) =>
      exitCode shouldBe ExitCode.Success
      calls should contain("quit")
    }
  }

  it should "fail when the joined game cannot be found" in {
    runCli(gameMessages, _ => IO.pure(None)).attempt.asserting {
      case Left(e: IllegalStateException) => e.getMessage shouldBe s"Game $id not found"
      case other                          => fail(s"unexpected result $other")
    }
  }
