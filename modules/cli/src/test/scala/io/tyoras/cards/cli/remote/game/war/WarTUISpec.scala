package io.tyoras.cards.cli.remote.game.war

import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.std.Dispatcher
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Ref}
import io.tyoras.cards.cli.fixtures.*
import io.tyoras.cards.cli.remote.fakes.*
import io.tyoras.cards.cli.remote.game.war.WarTUI.{Msg, State}
import io.tyoras.cards.cli.remote.game.war.WarTUI.State.{Loading, RenderState}
import io.tyoras.cards.cli.tui.TUI.Message
import io.tyoras.cards.cli.tui.TUI.Message.Notification
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.game.war.model.{Elimination, PlayerGameState, Turn}
import io.tyoras.cards.domain.game.war.model.PlayerGameState.*
import layoutz.{Key, TestKit}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class WarTUISpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val ansi = "\u001b\\[[0-9;]*m".r

  private val dave        = user(4, "Dave")
  private val playerNames = Map(alice.id -> "Alice", bob.id -> "Bob", carol.id -> "Carol", dave.id -> "Dave")

  private val aceOfHearts  = Card(Heart, Ace())
  private val twoOfSpades  = Card(Spade, Two())
  private val tenOfClubs   = Card(Club, Ten())
  private val kingDiamonds = Card(Diamond, King())

  private def turn(n: Int): Turn = Turn.applyUnsafe(n)
  private val counts             = Map(alice.id -> Card.Count.applyUnsafe(2), bob.id -> Card.Count.applyUnsafe(3))
  private val chatMessages       = List[Message](Message.Chat("Bob", "hello"), Notification.Info("Bob joined"))

  private val init       = Init(turn(1), alice.id, ready = Set(bob.id), notReady = Set(alice.id))
  private val battle     = BattleTurn(turn(2), alice.id, List(aceOfHearts, twoOfSpades), counts, Map(bob.id -> tenOfClubs), Set(alice.id))
  private val battleDone = battle.copy(playedCards = Map(bob.id -> tenOfClubs, alice.id -> aceOfHearts), missingPlays = Set.empty)
  private val warRound   = WarTurn.BattleRound(NonEmptySet.of(alice.id, bob.id), Map(bob.id -> twoOfSpades), Map(bob.id -> kingDiamonds))
  private val war        = WarTurn(turn(3), alice.id, List(tenOfClubs), counts, NonEmptyList.of(warRound, warRound), Set(alice.id), Set(alice.id))
  private val warVisible = war.copy(missingHidden = Set.empty)
  private val warDone    = war.copy(missingHidden = Set.empty, missingPlays = Set(bob.id))
  private val winTurn    = PlayerWinTurn(turn(4), alice.id, Nil, counts, bob.id, Set(aceOfHearts, kingDiamonds), Set(carol.id), Set(bob.id), Set(alice.id))
  private val finish    = Finish(turn(5), alice.id, alice.id, List(Elimination(bob.id, turn(2)), Elimination(carol.id, turn(3)), Elimination(dave.id, turn(4))))
  private val exitState = Exit(turn(6), alice.id)

  private case class Env(tui: WarTUI[IO], calls: Ref[IO, List[String]], gameState: Ref[IO, Option[PlayerGameState]])

  private def withTUI[A](initialState: Option[PlayerGameState] = Some(init), failing: Boolean = false)(f: Env => IO[A]): IO[A] =
    for
      calls     <- Ref.of[IO, List[String]](Nil)
      gameState <- Ref.of[IO, Option[PlayerGameState]](initialState)
      messages  <- Ref.of[IO, List[Message]](chatMessages)
      warClient  = FakeWarClient(gameId(1), Nil, calls, failing)
      chatClient = FakeChatClient(Nil, calls, failing)
      result <- Dispatcher.sequential[IO].use { d =>
        f(Env(WarTUI.make[IO]("WAR BANNER", alice.id, playerNames, gameState, messages, warClient, chatClient)(d), calls, gameState))
      }
    yield result

  private def rendered(gs: PlayerGameState): RenderState = RenderState(gs, tack = 0)

  private def view(env: Env, state: State): String = ansi.replaceAllIn(env.tui.view(state).render, "")

  "WarTUI init" should "start loading the game state" in {
    withTUI() { env =>
      IO(env.tui.init).asserting { (state, cmd) =>
        state shouldBe Loading()
        TestKit.titles(cmd) shouldBe List(s"War game - ${gameId(1)}")
        TestKit.hasHideCursor(cmd) shouldBe true
        TestKit.run(cmd) shouldBe List(Msg.GameStateLoaded(init, chatMessages))
      }
    }
  }

  "WarTUI update" should "increment the tick" in {
    withTUI() { env =>
      IO((env.tui.update(Msg.Tick, Loading(tack = 1))._1, env.tui.update(Msg.Tick, RenderState(init, tack = 3))._1)).asserting { (loading, render) =>
        loading.tick shouldBe 2
        render.tick shouldBe 4
      }
    }
  }

  it should "add notifications to the messages" in {
    val notif = Notification.Error("oops")
    withTUI() { env =>
      IO((env.tui.update(Msg.DisplayNotif(notif), Loading())._1, env.tui.update(Msg.DisplayNotif(notif), rendered(init))._1)).asserting { (loading, render) =>
        loading.messages shouldBe List(notif)
        render.messages shouldBe List(notif)
      }
    }
  }

  it should "type and remove chars of the current chat message" in {
    withTUI() { env =>
      IO {
        val loading = env.tui.update(Msg.TypeChar('b'), env.tui.update(Msg.TypeChar('a'), Loading())._1)._1
        val render  = env.tui.update(Msg.TypeChar('c'), RenderState(init, tack = 0, crtMsg = "ab"))._1
        (loading, render, env.tui.update(Msg.Backspace, loading)._1, env.tui.update(Msg.Backspace, render)._1)
      }.asserting { (loading, render, loadingErased, renderErased) =>
        loading.currentMsg shouldBe "ab"
        render.currentMsg shouldBe "abc"
        loadingErased.currentMsg shouldBe "a"
        renderErased.currentMsg shouldBe "ab"
      }
    }
  }

  it should "submit the chat message and empty the current message" in {
    withTUI() { env =>
      for
        (loadingState, loadingCmd) <- IO(env.tui.update(Msg.SubmitChatMessage("hi"), Loading(crtMsg = "hi")))
        (renderState, renderCmd)   <- IO(env.tui.update(Msg.SubmitChatMessage("yo"), RenderState(init, tack = 0, crtMsg = "yo")))
        msgs                       <- IO(TestKit.run(loadingCmd) ++ TestKit.run(renderCmd))
        calls                      <- env.calls.get
      yield
        loadingState.currentMsg shouldBe ""
        renderState.currentMsg shouldBe ""
        msgs shouldBe List(Msg.LoadGameState, Msg.LoadGameState)
        calls shouldBe List("chat hi", "chat yo")
    }
  }

  it should "notify when the chat message cannot be sent" in {
    withTUI(failing = true) { env =>
      IO(TestKit.run(env.tui.update(Msg.SubmitChatMessage("hi"), Loading(crtMsg = "hi"))._2)).asserting {
        _ shouldBe List(Msg.DisplayNotif(Notification.Error("Failed to send chat message: hi")))
      }
    }
  }

  it should "exit" in {
    withTUI() { env =>
      IO(env.tui.update(Msg.Exit, Loading())).asserting { (state, cmd) =>
        state shouldBe Loading()
        TestKit.hasExit(cmd) shouldBe true
      }
    }
  }

  it should "render the loaded game state while keeping the chat state" in {
    withTUI() { env =>
      IO(env.tui.update(Msg.GameStateLoaded(battle, chatMessages), Loading(tack = 5, crtMsg = "abc", active = true))._1).asserting {
        _ shouldBe RenderState(battle, chatMessages, 5, "abc", writingInChat = true)
      }
    }
  }

  it should "ignore game messages while loading" in {
    withTUI() { env =>
      IO(env.tui.update(Msg.SendReady, Loading(tack = 1))).asserting { (state, cmd) =>
        state shouldBe Loading(tack = 1)
        TestKit.isNone(cmd) shouldBe true
      }
    }
  }

  it should "reload the game state" in {
    withTUI(initialState = Some(battle)) { env =>
      IO(env.tui.update(Msg.LoadGameState, RenderState(init, tack = 3, crtMsg = "ab", writingInChat = true))).asserting { (state, cmd) =>
        state shouldBe Loading(crtMsg = "ab", active = true)
        TestKit.run(cmd) shouldBe List(Msg.GameStateLoaded(battle, chatMessages))
      }
    }
  }

  it should "send the ready input then reload the state" in {
    withTUI() { env =>
      for
        (state, cmd) <- IO(env.tui.update(Msg.SendReady, rendered(init)))
        msgs         <- IO(TestKit.run(cmd))
        calls        <- env.calls.get
      yield
        state shouldBe rendered(init)
        msgs shouldBe List(Msg.LoadGameState)
        calls shouldBe List("ready")
    }
  }

  it should "notify when the ready input cannot be sent" in {
    withTUI(failing = true) { env =>
      IO(TestKit.run(env.tui.update(Msg.SendReady, rendered(init))._2)).asserting {
        case List(Msg.DisplayNotif(Notification.Error(msg))) => msg should startWith("Failed to send ready input:")
        case other                                           => fail(s"unexpected messages $other")
      }
    }
  }

  it should "send the played card then reload the state" in {
    withTUI() { env =>
      for
        msgs  <- IO(TestKit.run(env.tui.update(Msg.SendPlayCard(aceOfHearts), rendered(battle))._2))
        calls <- env.calls.get
      yield
        msgs shouldBe List(Msg.LoadGameState)
        calls shouldBe List(s"play ${aceOfHearts.id}")
    }
  }

  it should "notify when the played card cannot be sent" in {
    withTUI(failing = true) { env =>
      IO(TestKit.run(env.tui.update(Msg.SendPlayCard(aceOfHearts), rendered(battle))._2)).asserting {
        case List(Msg.DisplayNotif(Notification.Error(msg))) => msg should startWith("Failed to send play card input:")
        case other                                           => fail(s"unexpected messages $other")
      }
    }
  }

  it should "toggle the chat focus" in {
    withTUI() { env =>
      IO(env.tui.update(Msg.ToggleChatFocus, rendered(init))._1).asserting(_.chatActive shouldBe true)
    }
  }

  it should "ignore unsupported messages once rendered" in {
    withTUI() { env =>
      IO(env.tui.update(Msg.MoveUp, rendered(init))).asserting { (state, cmd) =>
        state shouldBe rendered(init)
        TestKit.isNone(cmd) shouldBe true
      }
    }
  }

  "WarTUI subscriptions" should "only tick while loading" in {
    withTUI() { env =>
      IO(TestKit.timers(env.tui.subscriptions(Loading()))).asserting(_ shouldBe List(350L -> Msg.Tick))
    }
  }

  it should "tick and refresh the game state once rendered" in {
    withTUI() { env =>
      IO(TestKit.timers(env.tui.subscriptions(rendered(init)))).asserting(_ shouldBe List(350L -> Msg.Tick, 300L -> Msg.LoadGameState))
    }
  }

  it should "map the keys to game messages when the chat is inactive" in {
    withTUI() { env =>
      IO(TestKit.keyHandler(env.tui.subscriptions(rendered(init)))).asserting { handler =>
        handler(Key.Char('r')) shouldBe Some(Msg.LoadGameState)
        handler(Key.Tab) shouldBe Some(Msg.ToggleChatFocus)
        handler(Key.Char(' ')) shouldBe Some(Msg.SendReady)
        handler(Key.Enter) shouldBe Some(Msg.SendReady)
        handler(Key.Escape) shouldBe Some(Msg.Exit)
        handler(Key.Char('x')) shouldBe None
        handler(Key.Backspace) shouldBe None
      }
    }
  }

  it should "map the keys to chat messages when the chat is active" in {
    withTUI() { env =>
      IO {
        (
          TestKit.keyHandler(env.tui.subscriptions(RenderState(init, tack = 0, crtMsg = "hey", writingInChat = true))),
          TestKit.keyHandler(env.tui.subscriptions(RenderState(init, tack = 0, writingInChat = true)))
        )
      }.asserting { (typing, empty) =>
        typing(Key.Char('r')) shouldBe Some(Msg.TypeChar('r'))
        typing(Key.Char(' ')) shouldBe Some(Msg.TypeChar(' '))
        typing(Key.Backspace) shouldBe Some(Msg.Backspace)
        typing(Key.Enter) shouldBe Some(Msg.SubmitChatMessage("hey"))
        typing(Key.Tab) shouldBe Some(Msg.ToggleChatFocus)
        empty(Key.Backspace) shouldBe None
        empty(Key.Enter) shouldBe None
      }
    }
  }

  it should "send the right game input depending on the game state" in {
    def input(env: Env, state: State) = TestKit.keyHandler(env.tui.subscriptions(state))(Key.Char(' '))
    withTUI() { env =>
      IO {
        List(
          input(env, Loading()),
          input(env, rendered(init)),
          input(env, rendered(init.copy(ready = Set(alice.id, bob.id), notReady = Set.empty))),
          input(env, rendered(battle)),
          input(env, rendered(battleDone)),
          input(env, rendered(war)),
          input(env, rendered(warDone)),
          input(env, rendered(winTurn)),
          input(env, rendered(winTurn.copy(acked = Set(alice.id, bob.id), notAcked = Set.empty))),
          input(env, rendered(finish))
        )
      }.asserting {
        _ shouldBe List(
          None,
          Some(Msg.SendReady),
          None,
          Some(Msg.SendPlayCard(aceOfHearts)),
          None,
          Some(Msg.SendPlayCard(tenOfClubs)),
          None,
          Some(Msg.SendReady),
          None,
          None
        )
      }
    }
  }

  "WarTUI view" should "display a spinner while loading" in {
    withTUI() { env =>
      IO(view(env, Loading(msgs = chatMessages))).asserting { v =>
        v should (include("WAR BANNER").and(include("Loading...")).and(include("Messages")).and(include("hello")).and(include("Bob joined")))
        v should include("Use <tab> to switch between game and chat.")
      }
    }
  }

  it should "display the players readiness during the initialisation" in {
    withTUI() { env =>
      IO((view(env, rendered(init)), view(env, rendered(init.copy(ready = Set(alice.id, bob.id), notReady = Set.empty))))).asserting { (notReady, ready) =>
        notReady should (include("Waiting for everyone to be ready").and(include("Press <space> once you are ready to play...")).and(include("Not ready")))
        notReady should include("=== Ready to start ===")
        (ready should not).include("Press <space> once you are ready to play...")
      }
    }
  }

  it should "display the battle turn" in {
    withTUI() { env =>
      IO((view(env, rendered(battle)), view(env, rendered(battleDone)))).asserting { (toPlay, played) =>
        toPlay should (include("Turn 2 : Battle").and(include("Card to play")).and(include("Press <space> to play your card...")))
        toPlay should (include("=== Waiting cards from players ===").and(include("Waiting")))
        played should include("You have already played your card.")
      }
    }
  }

  it should "display the war turn" in {
    withTUI() { env =>
      IO((view(env, rendered(war)), view(env, rendered(warVisible)), view(env, rendered(warDone)))).asserting { (hidden, visible, done) =>
        hidden should (include("Turn 3 : War between").and(include("Round 1")).and(include("Round 2")))
        hidden should include("Press <space> to give a hidden card as prize for the war...")
        visible should include("Press <space> to play your next card...")
        done should include("You have already played your card.")
      }
    }
  }

  it should "display the result of a turn won by another player" in {
    withTUI() { env =>
      IO(view(env, rendered(winTurn))).asserting { v =>
        v should (include("Turn 3 : Result").and(include("Player Bob has won turn 3 and won 2 cards.")))
        v should (include("Players eliminated during this turn").and(include("Carol")).and(include("Eliminated")))
        v should (include("Press <space> once you are ready to play next turn...").and(include("=== Ready to play next turn ===")))
      }
    }
  }

  it should "display the result of a turn won by the current player" in {
    val won = winTurn.copy(winnerId = alice.id, eliminated = Set.empty, acked = Set(alice.id), notAcked = Set(bob.id))
    withTUI() { env =>
      IO(view(env, rendered(won))).asserting { v =>
        v should include("You have won turn 3")
        (v should not).include("Players eliminated during this turn")
        (v should not).include("Press <space> once you are ready to play next turn...")
      }
    }
  }

  it should "display the final ranking" in {
    withTUI() { env =>
      IO(view(env, rendered(finish))).asserting { v =>
        v should (include("Game finished after 4 turns.").and(include("The winner is")).and(include("Alice")))
        v should (include("Elimination turn").and(include("Bob")).and(include("Carol")).and(include("Dave")))
        v should include("Press <escape> to quit the game...")
      }
    }
  }

  it should "display the label of other states" in
    withTUI()(env => IO(view(env, rendered(exitState))).asserting(_ should include("current state: Exit")))
