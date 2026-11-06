package io.tyoras.cards.cli.tui

import cats.effect.std.Dispatcher
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Ref}
import io.tyoras.cards.cli.tui.TUI.Message
import io.tyoras.cards.cli.tui.TUI.Message.Notification
import layoutz.*
import layoutz.TestKit.ScriptedTerminal
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class TUISpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:
  import TUISpec.*

  private val ansi                      = "\u001b\\[[0-9;]*m".r
  private def plain(e: Element): String = ansi.replaceAllIn(e.render, "")

  "Message.Chat" should "render the sender and the text" in {
    val msg = Message.Chat("Alice", "Hello there")
    IO(plain(msg.tui)).asserting { r =>
      msg.text shouldBe "Hello there"
      r should (include("Alice : ").and(include("Hello there")))
    }
  }

  "Message.Notification" should "render info notifications" in {
    val notif = Notification.Info("Game started")
    IO(plain(notif.tui)).asserting { r =>
      notif.text shouldBe "Game started"
      r should (include("Info : ").and(include("Game started")))
    }
  }

  it should "render error notifications" in {
    val notif = Notification.Error("Boom")
    IO(plain(notif.tui)).asserting { r =>
      notif.text shouldBe "Boom"
      r should (include("Error : ").and(include("Boom")))
    }
  }

  "appWidth" should "be 120 chars" in
    IO(TUI.appWidth).asserting(_ shouldBe 120)

  "TUI helpers" should "render the app layout with the page banner" in {
    withTestTUI { tui =>
      IO(plain(tui.layoutWith("some content"))).asserting(r => r should (include("TEST BANNER").and(include("some content"))))
    }
  }

  it should "render an empty notification card when there is no notification" in
    withTestTUI(tui => IO(tui.notif(None)).asserting(_ shouldBe Empty))

  it should "render info and error notification cards" in {
    withTestTUI { tui =>
      IO((plain(tui.notif(Some(Notification.Info("all good")))), plain(tui.notif(Some(Notification.Error("bad")))))).asserting { (info, err) =>
        info should (include("Info").and(include("all good")))
        err should (include("Error").and(include("bad")))
      }
    }
  }

  it should "render the chat with its messages and the current input" in {
    withTestTUI { tui =>
      IO(plain(tui.chat(List(Message.Chat("Bob", "hi"), Notification.Info("joined")), "typing", active = true))).asserting { r =>
        r should (include("Messages").and(include("Bob : ")).and(include("hi")).and(include("joined")).and(include("typing")))
      }
    }
  }

  it should "turn a successful effect into a message" in
    withTestTUI(tui => IO(TestKit.run(tui.runEffect(IO.pure("ok"))(e => s"error: $e")))).asserting(_ shouldBe List("ok"))

  it should "turn a failed effect into an error message" in
    withTestTUI(tui => IO(TestKit.run(tui.runEffect(IO.raiseError(new Exception("boom")))(e => s"error: $e")))).asserting(_ shouldBe List("error: boom"))

  it should "fire and forget an effect" in {
    for
      ref    <- Ref.of[IO, Int](0)
      result <- withTestTUI(tui => IO(TestKit.run(tui.fireEffect(ref.set(42)))) >> ref.get.iterateUntil(_ == 42).timeout(5.seconds))
    yield result shouldBe 42
  }

  "runTUI" should "run the app until it exits" in {
    val terminal = ScriptedTerminal(List(List(TestKit.ScriptedTerminal.key('x'), TestKit.ScriptedTerminal.key(ScriptedTerminal.Enter))))
    for
      pressed <- Ref.of[IO, List[String]](Nil)
      _       <- TUI.runTUI[IO](d => TestTUI(d, pressed), tickIntervalMs = 10, renderIntervalMs = 10, terminal = Some(terminal)).timeout(10.seconds)
      keys    <- pressed.get.iterateUntil(_.nonEmpty).timeout(5.seconds)
    yield
      keys shouldBe List("x")
      terminal.output.mkString should include("TEST BANNER")
  }

  private def withTestTUI[A](f: TestTUI => IO[A]): IO[A] =
    Ref.of[IO, List[String]](Nil).flatMap(ref => Dispatcher.sequential[IO].use(d => f(TestTUI(d, ref))))

object TUISpec:
  class TestTUI(d: Dispatcher[IO], pressed: Ref[IO, List[String]]) extends TUI[IO, List[String], String]:
    override protected def dispatcher: Dispatcher[IO] = d
    override protected def pageBanner: String         = "TEST BANNER"

    def layoutWith(e: Element*): Element                                         = appLayout(e*)
    def notif(n: Option[Notification]): Element                                  = notificationCard(n)
    def chat(messages: List[Message], current: String, active: Boolean): Element = chatElement(messages, current, active)
    def runEffect(effect: IO[String])(err: String => String): Cmd[String]        = runEffectCmd(effect)(err)
    def fireEffect(effect: IO[Unit]): Cmd[String]                                = fireEffectCmd(effect)

    override def init: (List[String], Cmd[String]) = (Nil, Cmd.none)

    override def update(msg: String, state: List[String]): (List[String], Cmd[String]) =
      msg match
        case "exit" => state          -> Cmd.batch(fireEffectCmd(pressed.set(state)), Cmd.exit)
        case key    => (state :+ key) -> Cmd.none

    override def subscriptions(state: List[String]): Sub[String] =
      Sub.onKeyPress {
        case Key.Enter   => Some("exit")
        case Key.Char(c) => Some(c.toString)
        case _           => None
      }

    override def view(state: List[String]): Element = appLayout(Text(state.mkString(",")))
