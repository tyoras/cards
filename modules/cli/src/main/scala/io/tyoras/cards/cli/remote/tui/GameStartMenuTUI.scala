package io.tyoras.cards.cli.remote.tui

import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.kernel.Deferred
import cats.effect.std.Dispatcher
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.cli
import io.tyoras.cards.cli.remote.auth.AuthProvider
import io.tyoras.cards.cli.remote.client.{GamesClient, UsersClient}
import io.tyoras.cards.cli.remote.tui.GameStartMenuTUI.*
import io.tyoras.cards.cli.remote.tui.GameStartMenuTUI.MenuSelection.*
import io.tyoras.cards.cli.remote.tui.GameStartMenuTUI.Mode.*
import io.tyoras.cards.cli.tui.TUI
import io.tyoras.cards.domain.user.User
import layoutz.*
import cats.syntax.all.*
import io.tyoras.cards.cli.tui.TUI.Message.Notification
import io.tyoras.cards.domain.game.GameType

import scala.util.Try

trait GameStartMenuTUI[F[_]] extends TUI[F, State, Msg]

object GameStartMenuTUI:
  final case class State(mode: Mode)

  enum Mode:
    case Menu(selection: MenuSelection = Create, notif: Option[Notification] = None)
    case Creation(playerId: FUUID, availablePlayers: Seq[User.Existing], position: Int = 0, selection: Set[Int] = Set.empty)

  enum MenuSelection(val label: String):
    case Create extends MenuSelection("Create a new game")
    case Join   extends MenuSelection("Join a game created by a friend")

  enum Msg:
    case MoveUp
    case MoveDown
    case ToggleSelection
    case PlayersFound(players: Seq[User.Existing], playerId: FUUID)
    case Submit
    case DisplayNotif(notif: Notification)
    case Output(gameId: FUUID)
    case Back

  def make[F[_] : Async](
      gameType: GameType,
      banner: String,
      authProvider: AuthProvider[F],
      userClient: UsersClient[F],
      gameClient: GamesClient[F],
      gameId: Deferred[F, FUUID]
  ): Dispatcher[F] => GameStartMenuTUI[F] = disp =>
    new GameStartMenuTUI[F]:
      override protected def pageBanner: String        = banner
      override protected def dispatcher: Dispatcher[F] = disp

      override def init: (State, Cmd[Msg]) =
        State(Mode.Menu())

      override def update(msg: Msg, state: State): (State, Cmd[Msg]) =
        (msg, state.mode) match
          case (Msg.DisplayNotif(notif), _) => State(Menu(notif = notif.some)) -> Cmd.none
          case (Msg.MoveUp | Msg.MoveDown, menu: Menu) =>
            val selection = menu.selection match
              case Create => Join
              case Join   => Create
            State(Menu(selection)) -> Cmd.none
          case (Msg.MoveUp, creation: Creation) =>
            val position =
              if creation.position > 0
              then creation.position - 1
              else creation.availablePlayers.length - 1
            State(creation.copy(position = position)) -> Cmd.none
          case (Msg.MoveDown, creation: Creation) =>
            val position =
              if creation.position < creation.availablePlayers.length - 1
              then creation.position + 1
              else 0
            State(creation.copy(position = position)) -> Cmd.none
          case (Msg.ToggleSelection, creation: Creation) =>
            val selection =
              if creation.selection.contains(creation.position)
              then creation.selection - creation.position
              else creation.selection + creation.position
            State(creation.copy(selection = selection)) -> Cmd.none
          case (Msg.Submit, Menu(Create, _)) =>
            state -> runEffectCmd(listPlayers)(e => Msg.DisplayNotif(Notification.Error(s"Client error : $e")))
          case (Msg.Submit, Menu(Join, _)) =>
            state -> runEffectCmd(joinGame)(e => Msg.DisplayNotif(Notification.Error(s"Client error : $e")))
          case (Msg.Submit, creation: Creation) =>
            state -> runEffectCmd(createGame(creation))(e => Msg.DisplayNotif(Notification.Error(s"Client error : $e")))
          case (Msg.PlayersFound(players, playerId), _) =>
            State(Creation(playerId, availablePlayers = players, selection = Set(players.lastIndexWhere(_.id == playerId)))) -> Cmd.none
          case (Msg.Back, _) => State(Menu()) -> Cmd.none
          case (Msg.Output(id), _) =>
            state -> Cmd.batch(
              fireEffectCmd(gameId.complete(id)),
              Cmd.exit
            )
          case (_, _) => state -> Cmd.none

      private val listPlayers: F[Msg] =
        for
          creds   <- authProvider.connectedUserCredentials
          players <- userClient.listAll
        yield if players.size < 1 then Msg.DisplayNotif(Notification.Error("No players found")) else Msg.PlayersFound(players, creds.userId)

      private val joinGame: F[Msg] =
        for
          creds       <- authProvider.connectedUserCredentials
          activeGames <- gameClient.findByUserId(creds.userId, finished = false)
          foundGame = activeGames.sortBy(_.updatedAt).findLast(_.gameType == gameType)
        yield foundGame.fold(Msg.DisplayNotif(Notification.Error(s"No ${gameType.label} game found... Please try joining again after a moment.")))(g =>
          Msg.Output(g.id)
        )

      private def createGame(creation: Creation): F[Msg] =
        validatePlayerSelection(creation).fold(e => Msg.DisplayNotif(Notification.Error(e)).pure, gameClient.createWarGame(_).map(g => Msg.Output(g.id)))

      private def validatePlayerSelection(creation: Creation): Either[String, NonEmptyList[FUUID]] =
        for
          selected <- Try(creation.selection.map(creation.availablePlayers(_).id).toList).toEither.leftMap(e =>
            s"Unexpected selection problem: ${e.getMessage}"
          )
          _ <- Either.cond(selected.size >= gameType.minPlayers, (), s"You need to select at least ${gameType.minPlayers} player(s)")
          _ <- Either.cond(selected.size < gameType.maxPlayers, (), s"You have selected more than $gameType.maxPlayers player(s)")
          _ <- Either.cond(selected.contains(creation.playerId), (), s"You must be included in the selected players list")
        yield NonEmptyList.fromListUnsafe(selected)

      override def subscriptions(state: State): Sub[Msg] =
        Sub.onKeyPress {
          case Key.Up | Key.Char('k')   => Msg.MoveUp.some
          case Key.Down | Key.Char('j') => Msg.MoveDown.some
          case Key.Char(' ') =>
            state.mode match
              case _: Creation => Msg.ToggleSelection.some
              case _: Menu     => Msg.Submit.some
          case Key.Enter                                       => Msg.Submit.some
          case Key.Escape if state.mode.isInstanceOf[Creation] => Msg.Back.some
          case _                                               => None
        }

      override def view(state: State): Element =
        appLayout(
          br,
          state.mode match
            case Menu(selection, notif) =>
              section("Menu")(
                layout(
                  notificationCard(notif),
                  SingleChoice(
                    label = "What do you want to do?",
                    options = MenuSelection.values.map(_.label).toSeq,
                    selected = selection.ordinal,
                    active = true
                  )
                )
              ).center()
            case Creation(_, available, position, selection) =>
              section("Create new game")(
                MultiChoice(
                  label = s"Choose $playerChoiceLabel to play with",
                  options = available.map(_.name),
                  selected = selection,
                  cursor = position,
                  active = true
                )
              ).center()
        )

      private val playerChoiceLabel: String =
        if gameType.maxPlayers == 2 then "a friend" else s"up to ${gameType.maxPlayers - 1} friends"
