package io.tyoras.cards.cli.tui

import cats.effect.Async
import cats.effect.std.Dispatcher
import io.tyoras.cards.cli.tui.TUI.{Message, appWidth}
import layoutz.Color.*
import layoutz.*
import layoutz.Border.Round
import layoutz.Style.Bold

//TODO include a type parameter for the Output ???
abstract class TUI[F[_], State, Msg] extends LayoutzApp[State, Msg]:
  protected def dispatcher: Dispatcher[F]
  protected def pageBanner: String

  protected def fireEffectCmd[A](effect: F[A]): Cmd[Msg]                      = Cmd.fire(dispatcher.unsafeRunAndForget(effect))
  protected def runEffectCmd(effect: F[Msg])(errMsg: String => Msg): Cmd[Msg] = Cmd.task(dispatcher.unsafeRunSync(effect)) {
    case Right(value) => value
    case Left(err)    => errMsg(err)
  }

  protected def appLayout(elements: layoutz.Element*): layoutz.Element = {
    box("")(
      layout(
        banner(Text(pageBanner)).border(Border.Double).bg(Black).color(BrightGreen).center() +: elements*
      ).center(appWidth)
    ).bg(BrightGreen).color(BrightBlack)
  }

  protected def notificationCard(notif: Option[TUI.Message.Notification]): Element =
    notif.fold(Empty) {
      case TUI.Message.Notification.Info(info) => statusCard(Text("Info"), Text(info))
      case TUI.Message.Notification.Error(err) => statusCard(Text("Error"), Text(err)).bg(Yellow).color(Black)
    }

  protected def chatElement(messages: List[Message], currentMsg: String, active: Boolean): Element =
    box("Messages")(
      layout(
        messages.map(_.tui) :+
          textInput("Message", currentMsg, "", active)*
      )
    ).border(Round)

object TUI:
  val appWidth: Int = 120 // chars

  sealed trait Message:
    def text: String
    def tui: Element

  object Message:
    val width = 50
    final case class Chat(from: String, override val text: String) extends Message:
      override def tui: Element = rowTight(Text(s"$from : ").color(Blue), Text(text).wrap(width - from.length - 3))
    enum Notification(override val text: String) extends Message:
      case Error(msg: String) extends Notification(msg)
      case Info(msg: String)  extends Notification(msg)

      override def tui: Element = this match
        case Info(msg)  => rowTight(Text("Info : ").style(Bold), msg.wrap(width - 7)).color(Blue)
        case Error(msg) => rowTight(Text("Error : "), msg.wrap(width - 8)).color(BrightRed)

  // TODO return a Deferred output ?
  def runTUI[F[_] : Async](
      tui: Dispatcher[F] => TUI[F, ?, ?],
      tickIntervalMs: Long = 100,
      renderIntervalMs: Long = 50,
      alignment: Alignment = Alignment.Center
  ): F[Unit] =
    Dispatcher.sequential.use { dispatcher =>
      Async[F].delay(tui(dispatcher).run(tickIntervalMs, renderIntervalMs, alignment = alignment))
    }
