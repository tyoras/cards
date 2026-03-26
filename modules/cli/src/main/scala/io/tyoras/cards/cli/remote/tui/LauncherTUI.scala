package io.tyoras.cards.cli.remote.tui

import cats.effect.kernel.Deferred
import cats.effect.std.Dispatcher
import io.tyoras.cards.cli
import io.tyoras.cards.cli.remote.tui.LauncherTUI.*
import io.tyoras.cards.cli.tui.TUI
import io.tyoras.cards.domain.game.GameTyp.{Schnapsen, War}
import io.tyoras.cards.domain.game.GameType
import layoutz.*

object LauncherTUI:
  final case class State(gameSelection: Int)

  enum Msg:
    case MoveUp
    case MoveDown
    case Submit

  def make[F[_]](gameChoice: Deferred[F, GameType]): Dispatcher[F] => LauncherTUI[F] =
    new LauncherTUI[F](gameChoice)(_)

class LauncherTUI[F[_]](gameChoice: Deferred[F, GameType])(override val dispatcher: Dispatcher[F]) extends TUI[F, State, Msg]:
  private val games = Seq(War, Schnapsen)

  override val pageBanner: String = cli.banner

  override def init: (State, Cmd[Msg]) = State(gameSelection = 0)

  override def update(msg: Msg, state: State): (State, Cmd[Msg]) =
    msg match
      case Msg.MoveUp =>
        val selection = if state.gameSelection > 0 then state.gameSelection - 1 else games.length - 1
        state.copy(gameSelection = selection)
      case Msg.MoveDown =>
        val selection = if state.gameSelection < games.length - 1 then state.gameSelection + 1 else 0
        state.copy(gameSelection = selection)
      case Msg.Submit => state -> Cmd.batch(fireEffectCmd(gameChoice.complete(games(state.gameSelection))), Cmd.exit)

  override def subscriptions(state: State): Sub[Msg] =
    Sub.onKeyPress {
      case Key.Up | Key.Char('k')   => Some(Msg.MoveUp)
      case Key.Down | Key.Char('j') => Some(Msg.MoveDown)
      case Key.Enter                => Some(Msg.Submit)
      case _                        => None
    }

  override def view(state: State): Element =
    appLayout(
      section(s"Welcome!")(
        SingleChoice(
          label = "Which card game do you want to play?",
          options = games.map(_.label.capitalize),
          selected = state.gameSelection,
          active = true
        )
      ).center()
    )
