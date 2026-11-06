package io.tyoras.cards.cli.remote.tui

import cats.data.NonEmptyList
import cats.effect.std.Dispatcher
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{Deferred, IO, Ref}
import io.tyoras.cards.cli.fixtures.*
import io.tyoras.cards.cli.remote.auth.AuthProvider
import io.tyoras.cards.cli.remote.client.Game
import io.tyoras.cards.cli.remote.fakes.*
import io.tyoras.cards.cli.remote.tui.GameStartMenuTUI.*
import io.tyoras.cards.cli.tui.TUI.Message.Notification
import io.tyoras.cards.domain.game.GameTyp.{Schnapsen, War}
import io.tyoras.cards.domain.game.{GameType, Game as DomainGame}
import io.tyoras.cards.domain.game.war.model.PlayerId
import io.tyoras.cards.domain.user.model.User
import layoutz.{Key, TestKit}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class GameStartMenuTUISpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val ansi = "\u001b\\[[0-9;]*m".r

  private val players = Seq(alice, bob, carol)

  private case class Env(tui: GameStartMenuTUI[IO], gameId: Deferred[IO, DomainGame.ID], created: Ref[IO, List[NonEmptyList[PlayerId]]])

  private def withTUI[A](
      gameType: GameType = War,
      users: IO[List[User.Existing]] = IO.pure(players.toList),
      games: IO[List[Game]] = IO.pure(Nil),
      auth: AuthProvider[IO] = authProvider(),
      creationFails: Boolean = false
  )(f: Env => IO[A]): IO[A] =
    for
      gameId  <- Deferred[IO, DomainGame.ID]
      created <- Ref.of[IO, List[NonEmptyList[PlayerId]]](Nil)
      result  <- Dispatcher.sequential[IO].use { d =>
        val gamesClient = new FakeGamesClient(games, created):
          override def createWarGame(players: NonEmptyList[PlayerId]): IO[Game] =
            if creationFails then IO.raiseError(new Exception("creation refused")) else super.createWarGame(players)
        val tui = GameStartMenuTUI.make(gameType, "BANNER", auth, FakeUsersClient(users), gamesClient, gameId)(d)
        f(Env(tui, gameId, created))
      }
    yield result

  private def creation(position: Int = 0, selection: Set[Int] = Set.empty, available: Seq[User.Existing] = players) =
    Mode.Creation(alice.id, available, position, selection)

  private def submit(env: Env, state: State): IO[List[Msg]] =
    IO(env.tui.update(Msg.Submit, state)).flatMap { (newState, cmd) =>
      IO(newState shouldBe state) >> IO(TestKit.run(cmd))
    }

  "GameStartMenuTUI" should "start on the menu with game creation selected" in {
    withTUI() { env =>
      IO(env.tui.init).asserting { (state, cmd) =>
        state shouldBe State(Mode.Menu(MenuSelection.Create, None))
        TestKit.isNone(cmd) shouldBe true
      }
    }
  }

  it should "expose menu selection labels" in
    IO(MenuSelection.values.map(_.label).toList).asserting(_ shouldBe List("Create a new game", "Join a game created by a friend"))

  it should "toggle the menu selection when moving up or down" in {
    withTUI() { env =>
      IO {
        (
          env.tui.update(Msg.MoveUp, State(Mode.Menu(MenuSelection.Create)))._1,
          env.tui.update(Msg.MoveDown, State(Mode.Menu(MenuSelection.Join)))._1
        )
      }.asserting { (up, down) =>
        up shouldBe State(Mode.Menu(MenuSelection.Join))
        down shouldBe State(Mode.Menu(MenuSelection.Create))
      }
    }
  }

  it should "display a notification on the menu" in {
    withTUI() { env =>
      val notif = Notification.Error("oops")
      IO(env.tui.update(Msg.DisplayNotif(notif), State(creation()))._1).asserting(_ shouldBe State(Mode.Menu(notif = Some(notif))))
    }
  }

  it should "move the cursor up in the player list, wrapping to the last player" in {
    withTUI() { env =>
      IO((env.tui.update(Msg.MoveUp, State(creation(0)))._1, env.tui.update(Msg.MoveUp, State(creation(2)))._1)).asserting { (wrapped, up) =>
        wrapped shouldBe State(creation(2))
        up shouldBe State(creation(1))
      }
    }
  }

  it should "move the cursor down in the player list, wrapping to the first player" in {
    withTUI() { env =>
      IO((env.tui.update(Msg.MoveDown, State(creation(2)))._1, env.tui.update(Msg.MoveDown, State(creation(0)))._1)).asserting { (wrapped, down) =>
        wrapped shouldBe State(creation(0))
        down shouldBe State(creation(1))
      }
    }
  }

  it should "toggle the selection of the player under the cursor" in {
    withTUI() { env =>
      IO {
        (
          env.tui.update(Msg.ToggleSelection, State(creation(1, Set(0))))._1,
          env.tui.update(Msg.ToggleSelection, State(creation(1, Set(0, 1))))._1
        )
      }.asserting { (selected, unselected) =>
        selected shouldBe State(creation(1, Set(0, 1)))
        unselected shouldBe State(creation(1, Set(0)))
      }
    }
  }

  it should "ignore messages that do not apply to the current mode" in {
    withTUI() { env =>
      val state = State(Mode.Menu(MenuSelection.Join))
      IO(env.tui.update(Msg.ToggleSelection, state)).asserting { (newState, cmd) =>
        newState shouldBe state
        TestKit.isNone(cmd) shouldBe true
      }
    }
  }

  it should "go back to the menu" in
    withTUI()(env => IO(env.tui.update(Msg.Back, State(creation(1, Set(1))))._1).asserting(_ shouldBe State(Mode.Menu())))

  it should "switch to the player selection, pre-selecting the current player" in {
    withTUI() { env =>
      IO(env.tui.update(Msg.PlayersFound(players, bob.id), State(Mode.Menu()))._1).asserting {
        _ shouldBe State(Mode.Creation(bob.id, players, 0, Set(1)))
      }
    }
  }

  it should "output the selected game id and exit" in {
    withTUI() { env =>
      val state = State(Mode.Menu())
      for
        (newState, cmd) <- IO(env.tui.update(Msg.Output(gameId(7)), state))
        _               <- IO(TestKit.run(cmd))
        id              <- env.gameId.get.timeout(5.seconds)
      yield
        newState shouldBe state
        TestKit.hasExit(cmd) shouldBe true
        id shouldBe gameId(7)
    }
  }

  "Submitting the game creation menu" should "list the available players" in
    withTUI()(env => submit(env, State(Mode.Menu(MenuSelection.Create))).asserting(_ shouldBe List(Msg.PlayersFound(players, alice.id))))

  it should "display an error when there is no player" in {
    withTUI(users = IO.pure(Nil)) { env =>
      submit(env, State(Mode.Menu(MenuSelection.Create))).asserting(_ shouldBe List(Msg.DisplayNotif(Notification.Error("No players found"))))
    }
  }

  it should "display an error when the players cannot be retrieved" in {
    withTUI(users = IO.raiseError(new Exception("server down"))) { env =>
      submit(env, State(Mode.Menu(MenuSelection.Create))).asserting {
        case List(Msg.DisplayNotif(Notification.Error(msg))) => msg should (startWith("Client error : ").and(include("server down")))
        case other                                           => fail(s"unexpected messages $other")
      }
    }
  }

  "Submitting the game join menu" should "output the most recently updated game of the right type" in {
    val players = NonEmptyList.of(alice.id, bob.id)
    val games   = List(
      game(1, War, players, now.minusDays(1)),
      game(2, War, players, now),
      game(3, Schnapsen, players, now.plusDays(1)),
      game(4, War, NonEmptyList.of(bob.id, carol.id), now.plusDays(2))
    )
    withTUI(games = IO.pure(games))(env => submit(env, State(Mode.Menu(MenuSelection.Join))).asserting(_ shouldBe List(Msg.Output(gameId(2)))))
  }

  it should "display an error when no game is found" in {
    withTUI() { env =>
      submit(env, State(Mode.Menu(MenuSelection.Join))).asserting {
        _ shouldBe List(Msg.DisplayNotif(Notification.Error("No war game found... Please try joining again after a moment.")))
      }
    }
  }

  it should "display an error when the user is not authenticated" in {
    withTUI(auth = unauthenticated) { env =>
      submit(env, State(Mode.Menu(MenuSelection.Join))).asserting {
        case List(Msg.DisplayNotif(Notification.Error(msg))) => msg should include("User is not authenticated")
        case other                                           => fail(s"unexpected messages $other")
      }
    }
  }

  "Submitting the player selection" should "create a game with the selected players" in {
    withTUI() { env =>
      for
        msgs    <- submit(env, State(creation(selection = Set(0, 2))))
        created <- env.created.get
      yield
        msgs shouldBe List(Msg.Output(gameId(99)))
        created.map(_.toList.toSet) shouldBe List(Set(alice.id, carol.id))
    }
  }

  it should "display an error when the game cannot be created" in {
    withTUI(creationFails = true) { env =>
      submit(env, State(creation(selection = Set(0, 1)))).asserting {
        case List(Msg.DisplayNotif(Notification.Error(msg))) => msg should (startWith("Client error : ").and(include("creation refused")))
        case other                                           => fail(s"unexpected messages $other")
      }
    }
  }

  it should "require the minimum number of players" in {
    withTUI() { env =>
      submit(env, State(creation(selection = Set(0))))
        .asserting(_ shouldBe List(Msg.DisplayNotif(Notification.Error("You need to select at least 2 player(s)"))))
    }
  }

  it should "reject more players than the maximum" in {
    val manyPlayers = (1 to 53).map(n => user(n, s"Player$n"))
    withTUI() { env =>
      submit(env, State(creation(selection = manyPlayers.indices.toSet, available = manyPlayers))).asserting {
        case List(Msg.DisplayNotif(Notification.Error(msg))) => msg should include("You have selected more than")
        case other                                           => fail(s"unexpected messages $other")
      }
    }
  }

  it should "require the current player to be selected" in {
    withTUI() { env =>
      submit(env, State(creation(selection = Set(1, 2)))).asserting {
        _ shouldBe List(Msg.DisplayNotif(Notification.Error("You must be included in the selected players list")))
      }
    }
  }

  it should "report an invalid selection" in {
    withTUI() { env =>
      submit(env, State(creation(selection = Set(0, 42)))).asserting {
        case List(Msg.DisplayNotif(Notification.Error(msg))) => msg should startWith("Unexpected selection problem:")
        case other                                           => fail(s"unexpected messages $other")
      }
    }
  }

  "GameStartMenuTUI subscriptions" should "map the keys to messages on the menu" in {
    withTUI() { env =>
      IO(TestKit.keyHandler(env.tui.subscriptions(State(Mode.Menu())))).asserting { handler =>
        handler(Key.Up) shouldBe Some(Msg.MoveUp)
        handler(Key.Char('k')) shouldBe Some(Msg.MoveUp)
        handler(Key.Down) shouldBe Some(Msg.MoveDown)
        handler(Key.Char('j')) shouldBe Some(Msg.MoveDown)
        handler(Key.Char(' ')) shouldBe Some(Msg.Submit)
        handler(Key.Enter) shouldBe Some(Msg.Submit)
        handler(Key.Escape) shouldBe None
        handler(Key.Char('x')) shouldBe None
      }
    }
  }

  it should "map the keys to messages in the player selection" in {
    withTUI() { env =>
      IO(TestKit.keyHandler(env.tui.subscriptions(State(creation())))).asserting { handler =>
        handler(Key.Char(' ')) shouldBe Some(Msg.ToggleSelection)
        handler(Key.Enter) shouldBe Some(Msg.Submit)
        handler(Key.Escape) shouldBe Some(Msg.Back)
      }
    }
  }

  "GameStartMenuTUI view" should "display the menu and its notification" in {
    withTUI() { env =>
      IO(ansi.replaceAllIn(env.tui.view(State(Mode.Menu(MenuSelection.Join, Some(Notification.Info("hello"))))).render, "")).asserting { view =>
        view should (include("BANNER").and(include("What do you want to do?")).and(include("Join a game created by a friend")).and(include("hello")))
      }
    }
  }

  it should "display the player selection for a multiplayer game" in {
    withTUI() { env =>
      IO(ansi.replaceAllIn(env.tui.view(State(creation(1, Set(0)))).render, "")).asserting { view =>
        view should (include("Create new game").and(include("up to 51 friends")).and(include("Alice")).and(include("Bob")).and(include("Carol")))
      }
    }
  }

  it should "display the player selection for a 2 players game" in {
    withTUI(gameType = Schnapsen) { env =>
      IO(ansi.replaceAllIn(env.tui.view(State(creation())).render, "")).asserting(_ should include("Choose a friend to play with"))
    }
  }
