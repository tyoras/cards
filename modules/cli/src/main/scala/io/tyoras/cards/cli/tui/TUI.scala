package io.tyoras.cards.cli.tui

import cats.effect.Async
import cats.effect.std.Dispatcher
import io.tyoras.cards.cli.tui.TUI.appWidth
import layoutz.Color.{Black, BrightBlack, BrightGreen, Yellow}
import layoutz.*

//TODO include a type parameter for the Output ???
abstract class TUI[F[_], State, Msg] extends LayoutzApp[State, Msg]:
  protected def dispatcher: Dispatcher[F]
  protected def pageBanner: String

  protected def fireEffectCmd[A](effect: F[A]): Cmd[Msg] = Cmd.fire(dispatcher.unsafeRunAndForget(effect))
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

  protected def notificationCard(notif: Option[TUI.Notification]): Element =
    notif.fold(Empty) {
      case TUI.Notification.Info(info) => statusCard(Text("Info"), Text(info))
      case TUI.Notification.Error(err) => statusCard(Text("Error"), Text(err)).bg(Yellow).color(Black)
    }

object TUI:
  val appWidth: Int = 120 // chars

  enum Notification(val message: String):
    case Error(msg: String) extends Notification(msg)
    case Info(msg: String)  extends Notification(msg)

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
