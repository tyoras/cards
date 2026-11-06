package io.tyoras.cards.cli.remote.tui

import cats.effect.std.Dispatcher
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{Deferred, IO}
import io.tyoras.cards.cli
import io.tyoras.cards.cli.remote.tui.LauncherTUI.{Msg, State}
import io.tyoras.cards.domain.game.GameTyp.{Schnapsen, War}
import io.tyoras.cards.domain.game.GameType
import layoutz.{Key, TestKit}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class LauncherTUISpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val ansi = "\u001b\\[[0-9;]*m".r

  private def withLauncher[A](f: (LauncherTUI[IO], Deferred[IO, GameType]) => IO[A]): IO[A] =
    Deferred[IO, GameType].flatMap(choice => Dispatcher.sequential[IO].use(d => f(LauncherTUI.make(choice)(d), choice)))

  "LauncherTUI" should "start with the first game selected and no command" in {
    withLauncher { (tui, _) =>
      IO(tui.init).asserting { (state, cmd) =>
        state shouldBe State(0)
        TestKit.isNone(cmd) shouldBe true
      }
    }
  }

  it should "use the cli banner" in
    withLauncher((tui, _) => IO(tui.pageBanner).asserting(_ shouldBe cli.banner))

  it should "cycle through the games when moving up" in {
    withLauncher { (tui, _) =>
      IO((tui.update(Msg.MoveUp, State(0))._1, tui.update(Msg.MoveUp, State(1))._1)).asserting { (fromFirst, fromLast) =>
        fromFirst shouldBe State(1)
        fromLast shouldBe State(0)
      }
    }
  }

  it should "cycle through the games when moving down" in {
    withLauncher { (tui, _) =>
      IO((tui.update(Msg.MoveDown, State(0))._1, tui.update(Msg.MoveDown, State(1))._1)).asserting { (fromFirst, fromLast) =>
        fromFirst shouldBe State(1)
        fromLast shouldBe State(0)
      }
    }
  }

  it should "complete the game choice and exit on submit" in {
    withLauncher { (tui, choice) =>
      for
        (state, cmd) <- IO(tui.update(Msg.Submit, State(1)))
        _            <- IO(TestKit.run(cmd))
        chosen       <- choice.get.timeout(5.seconds)
      yield
        state shouldBe State(1)
        TestKit.hasExit(cmd) shouldBe true
        chosen shouldBe Schnapsen
    }
  }

  it should "select War when submitting the first game" in {
    withLauncher { (tui, choice) =>
      IO(TestKit.run(tui.update(Msg.Submit, State(0))._2)) >> choice.get.timeout(5.seconds).asserting(_ shouldBe War)
    }
  }

  it should "map the keys to messages" in {
    withLauncher { (tui, _) =>
      IO(TestKit.keyHandler(tui.subscriptions(State(0)))).asserting { handler =>
        handler(Key.Up) shouldBe Some(Msg.MoveUp)
        handler(Key.Char('k')) shouldBe Some(Msg.MoveUp)
        handler(Key.Down) shouldBe Some(Msg.MoveDown)
        handler(Key.Char('j')) shouldBe Some(Msg.MoveDown)
        handler(Key.Enter) shouldBe Some(Msg.Submit)
        handler(Key.Char('x')) shouldBe None
      }
    }
  }

  it should "display the available games" in {
    withLauncher { (tui, _) =>
      IO(ansi.replaceAllIn(tui.view(State(1)).render, "")).asserting { view =>
        view should (include("Welcome!").and(include("Which card game do you want to play?")).and(include("War")).and(include("Schnapsen")))
      }
    }
  }
