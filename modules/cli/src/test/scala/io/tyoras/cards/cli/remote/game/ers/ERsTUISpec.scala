package io.tyoras.cards.cli.remote.game.ers

import cats.data.NonEmptyList
import cats.effect.std.Dispatcher
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Ref}
import io.tyoras.cards.cli.fixtures.*
import io.tyoras.cards.cli.remote.fakes.*
import io.tyoras.cards.cli.remote.game.ers.ERsTUI.{Msg, State}
import io.tyoras.cards.cli.remote.game.ers.ERsTUI.State.{Loading, RenderState}
import io.tyoras.cards.cli.tui.TUI.Message
import io.tyoras.cards.cli.tui.TUI.Message.Notification
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.{Card, Turn}
import io.tyoras.cards.domain.game.ers.model.{Elimination, PlayerGameState}
import io.tyoras.cards.domain.game.ers.model.PlayerGameState.*
import layoutz.{Key, TestKit}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class ERsTUISpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val ansi = "\u001b\\[[0-9;]*m".r

  private val dave        = user(4, "Dave")
  private val playerNames = Map(alice.id -> "Alice", bob.id -> "Bob", carol.id -> "Carol", dave.id -> "Dave")

  private val aceOfHearts  = Card(Heart, Ace())
  private val twoOfSpades  = Card(Spade, Two())
  private val kingDiamonds = Card(Diamond, King())

  private def turn(n: Int): Turn          = Turn.applyUnsafe(n)
  private def count(n: Int): Card.Count   = Card.Count.applyUnsafe(n)
  private val counts                      = Map(alice.id -> count(2), bob.id -> count(3))
  private val chatMessages: List[Message] = List(Message.Chat("Bob", "hello"), Notification.Info("Bob joined"))

  private val init        = Init(turn(1), alice.id, ready = Set(bob.id), notReady = Set(alice.id))
  private val myTurn      = PlayerTurn(turn(2), alice.id, alice.id, Some(twoOfSpades), count(1), count(2), counts)
  private val bobTurn     = myTurn.copy(waitedPlayer = bob.id, pileTopCard = None, pileSize = count(0))
  private val challenged  = ChallengeTurn(turn(2), alice.id, bob.id, alice.id, 2, 1, kingDiamonds, count(3), count(2), counts)
  private val challenging = challenged.copy(challenger = alice.id, challenged = bob.id)
  private val myPenalty   = SlapPenalty(
    turn = turn(2),
    playerId = alice.id,
    penalizedPlayer = alice.id,
    pileTopCard = twoOfSpades,
    pileSize = count(2),
    handTopCard = Some(aceOfHearts),
    handSize = count(2),
    opponents = counts,
    discardedCards = Nil,
    remainingCardsToDiscard = count(1),
    acked = Set.empty,
    notAcked = Set(bob.id)
  )
  private val bobPenalty = myPenalty.copy(penalizedPlayer = bob.id, notAcked = Set(alice.id))
  private val winTurn    =
    PlayerWinTurn(turn(3), alice.id, count(2), counts, bob.id, NonEmptyList.of(aceOfHearts, kingDiamonds), Set(carol.id), Set(bob.id), Set(alice.id))
  private val finish    = Finish(turn(5), alice.id, alice.id, List(Elimination(bob.id, turn(2)), Elimination(carol.id, turn(3)), Elimination(dave.id, turn(4))))
  private val exitState = Exit(turn(6), alice.id)

  private case class Env(tui: ERsTUI[IO], calls: Ref[IO, List[String]], gameState: Ref[IO, Option[PlayerGameState]])

  private def withTUI[A](initialState: Option[PlayerGameState] = Some(init), failing: Boolean = false)(f: Env => IO[A]): IO[A] =
    for
      calls     <- Ref.of[IO, List[String]](Nil)
      gameState <- Ref.of[IO, Option[PlayerGameState]](initialState)
      messages  <- Ref.of[IO, List[Message]](chatMessages)
      ersClient  = FakeERsClient(gameId(1), Nil, calls, failing)
      chatClient = FakeChatClient(Nil, calls, failing)
      result <- Dispatcher.sequential[IO].use { d =>
        f(Env(ERsTUI.make[IO]("ERS BANNER", alice.id, playerNames, gameState, messages, ersClient, chatClient)(d), calls, gameState))
      }
    yield result

  private def rendered(gs: PlayerGameState): RenderState = RenderState(gs, tack = 0)

  private def view(env: Env, state: State): String = ansi.replaceAllIn(env.tui.view(state).render, "")

  "ERsTUI init" should "start loading the game state" in {
    withTUI() { env =>
      IO(env.tui.init).asserting { (state, cmd) =>
        state shouldBe Loading()
        TestKit.titles(cmd) shouldBe List(s"Egyptian ratscrew game - ${gameId(1)}")
        TestKit.hasHideCursor(cmd) shouldBe true
        TestKit.run(cmd) shouldBe List(Msg.GameStateLoaded(init, chatMessages))
      }
    }
  }

  "ERsTUI update" should "increment the tick" in {
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
      IO(env.tui.update(Msg.GameStateLoaded(myTurn, chatMessages), Loading(tack = 5, crtMsg = "abc", active = true))._1).asserting {
        _ shouldBe RenderState(myTurn, chatMessages, 5, "abc", writingInChat = true)
      }
    }
  }

  it should "ignore game messages while loading" in {
    withTUI() { env =>
      IO(List(Msg.SendReady, Msg.SendSlap, Msg.SendPlayCard, Msg.SendDiscard(aceOfHearts)).map(env.tui.update(_, Loading(tack = 1)))).asserting { results =>
        results.map(_._1) shouldBe List.fill(4)(Loading(tack = 1))
        results.map((_, cmd) => TestKit.isNone(cmd)) shouldBe List.fill(4)(true)
      }
    }
  }

  it should "reload the game state" in {
    withTUI(initialState = Some(myTurn)) { env =>
      IO(env.tui.update(Msg.LoadGameState, RenderState(init, tack = 3, crtMsg = "ab", writingInChat = true))).asserting { (state, cmd) =>
        state shouldBe Loading(crtMsg = "ab", active = true)
        TestKit.run(cmd) shouldBe List(Msg.GameStateLoaded(myTurn, chatMessages))
      }
    }
  }

  it should "send the game inputs then reload the state" in {
    withTUI() { env =>
      for
        ready   <- IO(TestKit.run(env.tui.update(Msg.SendReady, rendered(init))._2))
        play    <- IO(TestKit.run(env.tui.update(Msg.SendPlayCard, rendered(myTurn))._2))
        slap    <- IO(TestKit.run(env.tui.update(Msg.SendSlap, rendered(bobTurn))._2))
        discard <- IO(TestKit.run(env.tui.update(Msg.SendDiscard(aceOfHearts), rendered(myPenalty))._2))
        calls   <- env.calls.get
      yield
        List(ready, play, slap, discard).flatten shouldBe List.fill(4)(Msg.LoadGameState)
        calls shouldBe List("ready", "play", "slap", s"discard ${aceOfHearts.id}")
    }
  }

  it should "notify when the game inputs cannot be sent" in {
    def error(env: Env, msg: Msg, state: PlayerGameState): IO[String] =
      IO(TestKit.run(env.tui.update(msg, rendered(state))._2)).map {
        case List(Msg.DisplayNotif(Notification.Error(text))) => text
        case other                                            => fail(s"unexpected messages $other")
      }
    withTUI(failing = true) { env =>
      for
        ready   <- error(env, Msg.SendReady, init)
        play    <- error(env, Msg.SendPlayCard, myTurn)
        slap    <- error(env, Msg.SendSlap, bobTurn)
        discard <- error(env, Msg.SendDiscard(aceOfHearts), myPenalty)
      yield
        ready should startWith("Failed to send ready input:")
        play should startWith("Failed to send play card input:")
        slap should startWith("Failed to send slap input:")
        discard should startWith("Failed to send discard card input:")
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

  "ERsTUI subscriptions" should "only tick while loading" in {
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
          TestKit.keyHandler(env.tui.subscriptions(RenderState(myTurn, tack = 0, crtMsg = "hey", writingInChat = true))),
          TestKit.keyHandler(env.tui.subscriptions(RenderState(myTurn, tack = 0, writingInChat = true)))
        )
      }.asserting { (typing, empty) =>
        typing(Key.Char('s')) shouldBe Some(Msg.TypeChar('s'))
        typing(Key.Char(' ')) shouldBe Some(Msg.TypeChar(' '))
        typing(Key.Backspace) shouldBe Some(Msg.Backspace)
        typing(Key.Enter) shouldBe Some(Msg.SubmitChatMessage("hey"))
        typing(Key.Tab) shouldBe Some(Msg.ToggleChatFocus)
        empty(Key.Backspace) shouldBe None
        empty(Key.Enter) shouldBe None
      }
    }
  }

  it should "send the right game input with <space> depending on the game state" in {
    def input(env: Env, state: State) = TestKit.keyHandler(env.tui.subscriptions(state))(Key.Char(' '))
    withTUI() { env =>
      IO {
        List(
          input(env, Loading()),
          input(env, rendered(init)),
          input(env, rendered(init.copy(ready = Set(alice.id, bob.id), notReady = Set.empty))),
          input(env, rendered(myTurn)),
          input(env, rendered(bobTurn)),
          input(env, rendered(challenged)),
          input(env, rendered(challenging)),
          input(env, rendered(myPenalty)),
          input(env, rendered(myPenalty.copy(remainingCardsToDiscard = count(0), discardedCards = List(aceOfHearts)))),
          input(env, rendered(myPenalty.copy(handTopCard = None, handSize = count(0)))),
          input(env, rendered(bobPenalty)),
          input(env, rendered(bobPenalty.copy(acked = Set(alice.id), notAcked = Set.empty))),
          input(env, rendered(winTurn)),
          input(env, rendered(winTurn.copy(acked = Set(alice.id, bob.id), notAcked = Set.empty))),
          input(env, rendered(finish))
        )
      }.asserting {
        _ shouldBe List(
          None,
          Some(Msg.SendReady),
          None,
          Some(Msg.SendPlayCard),
          None,
          Some(Msg.SendPlayCard),
          None,
          Some(Msg.SendDiscard(aceOfHearts)),
          None,
          None,
          Some(Msg.SendReady),
          None,
          Some(Msg.SendReady),
          None,
          None
        )
      }
    }
  }

  it should "slap the pile with [s] only during the player and challenge turns" in {
    def slap(env: Env, state: State) = TestKit.keyHandler(env.tui.subscriptions(state))(Key.Char('s'))
    withTUI() { env =>
      IO {
        List(
          slap(env, Loading()),
          slap(env, rendered(init)),
          slap(env, rendered(myTurn)),
          slap(env, rendered(bobTurn)),
          slap(env, rendered(challenged)),
          slap(env, rendered(myPenalty)),
          slap(env, rendered(winTurn)),
          slap(env, rendered(finish))
        )
      }.asserting(_ shouldBe List(None, None, Some(Msg.SendSlap), Some(Msg.SendSlap), Some(Msg.SendSlap), None, None, None))
    }
  }

  "ERsTUI view" should "display a spinner while loading" in {
    withTUI() { env =>
      IO(view(env, Loading(msgs = chatMessages))).asserting { v =>
        v should (include("ERS BANNER").and(include("Loading...")).and(include("Messages")).and(include("hello")).and(include("Bob joined")))
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

  it should "display the player turn" in {
    withTUI() { env =>
      IO((view(env, rendered(myTurn)), view(env, rendered(bobTurn)))).asserting { (mine, bobs) =>
        mine should (include("Turn 2 : Battle").and(include("Your hand")).and(include("Pile")).and(include("last played")))
        mine should (include("Press <space> to play your card...").and(include("Press [S] to slap the pile !")))
        mine should include("=== Waiting card from player ===")
        (bobs should not).include("Press <space> to play your card...")
        (bobs should not).include("Press [S] to slap the pile !")
        bobs should include("Empty")
      }
    }
  }

  it should "display the challenge turn" in {
    withTUI() { env =>
      IO((view(env, rendered(challenged)), view(env, rendered(challenging)))).asserting { (mine, bobs) =>
        mine should (include("Turn 2 : Challenge").and(include("Bob has challenged Alice")).and(include("1/2 remaining tries")))
        mine should (include("Press <space> to play your card...").and(include("Press [S] to slap the pile !")))
        (bobs should not).include("Press <space> to play your card...")
      }
    }
  }

  it should "display the slap penalty" in {
    val payed = myPenalty.copy(remainingCardsToDiscard = count(0), discardedCards = List(aceOfHearts))
    withTUI() { env =>
      IO((view(env, rendered(myPenalty)), view(env, rendered(bobPenalty)), view(env, rendered(payed)))).asserting { (mine, bobs, done) =>
        mine should (include("Turn 2 : Slap penalty").and(include("You have slapped the pile")).and(include("discard 1 card(s)")))
        mine should (include("Press <space> to discard your card...").and(include("=== Waiting card from player ===")))
        (mine should not).include("Press [S] to slap the pile !")
        bobs should include("Bob has slapped the pile")
        (bobs should not).include("Press <space> to discard your card...")
        (done should not).include("=== Waiting card from player ===")
        done should include("=== Ready to play next turn ===")
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
