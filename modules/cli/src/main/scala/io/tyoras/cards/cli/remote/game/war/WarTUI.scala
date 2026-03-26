package io.tyoras.cards.cli.remote.game.war

import cats.effect.{Async, Ref}
import cats.effect.std.Dispatcher
import io.tyoras.cards.cli.remote.client.WarClient
import io.tyoras.cards.cli.remote.game.war.WarTUI.{Msg, State}
import io.tyoras.cards.cli.tui.*
import io.tyoras.cards.domain.game.war.model.{GameState, PlayerId}
import io.tyoras.cards.domain.game.war.*
import layoutz.*
import layoutz.Key.*
import cats.syntax.all.*
import io.tyoras.cards.cli.remote.game.war.WarTUI.Msg.Tick
import io.tyoras.cards.cli.remote.game.war.WarTUI.State.*
import io.tyoras.cards.cli.tui.TUI.Notification
import io.tyoras.cards.cli.tui.TUI.Notification.Error
import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.game.war.model.GameState.{BattleTurn, Finish, Init, PlayerWinTurn, WarTurn}
import layoutz.Border.Round
import layoutz.Color.{Blue, BrightWhite, Full, Red}
import layoutz.Style.{Blink, Bold}

trait WarTUI[F[_]] extends TUI[F, State, Msg]

object WarTUI:
  enum State(val notification: Option[Notification]):
    case Loading(tick: Int = 0, notif: Option[Notification] = None)            extends State(notif)
    case RenderState(gameState: GameState, notif: Option[Notification] = None) extends State(notif)

  enum Msg:
    case DisplayNotif(notif: Notification)
    case MoveUp
    case MoveDown
    case LoadGameState
    case GameStateLoaded(gameState: GameState, notif: Option[Notification])
    case SendReady
    case SendPlayCard(card: Card)
    case Tick
    case Exit

  def make[F[_] : Async](
      banner: String,
      playerId: PlayerId,
      playerNames: Map[PlayerId, String],
      internalState: Ref[F, Option[GameState]],
      notification: Ref[F, Option[Notification]],
      warClient: WarClient[F]
  ): Dispatcher[F] => WarTUI[F] = d =>
    new WarTUI[F]:
      override protected def pageBanner: String        = banner
      override protected def dispatcher: Dispatcher[F] = d

      override def init: (State, Cmd[Msg]) =
        Loading() -> Cmd.batch(
          Cmd.setTitle(s"War game - ${warClient.gameId}"),
          Cmd.hideCursor,
          runEffectCmd(loadState)(e => Msg.DisplayNotif(Error(s"Failed to load initial game state: $e")))
        )

      override def update(msg: Msg, state: State): (State, Cmd[Msg]) =
        state match
          case state @ Loading(tick, _) =>
            msg match
              case Msg.Tick                      => state.copy(tick = tick + 1) -> Cmd.none
              case Msg.GameStateLoaded(s, notif) => RenderState(s, notif)       -> Cmd.none
              case Msg.Exit                      => state                       -> Cmd.exit
              case _                             => state
          case state @ RenderState(gameState, notif) =>
            msg match
              case Msg.LoadGameState => Loading() -> runEffectCmd(loadState)(e => Msg.DisplayNotif(Notification.Error(s"Failed to load game state: $e")))
              case Msg.SendReady     => state     -> runEffectCmd(sendReady)(e => Msg.DisplayNotif(Notification.Error(s"Failed to send ready input: $e")))
              case Msg.SendPlayCard(card) =>
                state -> runEffectCmd(sendPlayCard(card))(e => Msg.DisplayNotif(Notification.Error(s"Failed to send play card input: $e")))
              case Msg.Exit => state -> Cmd.exit
              case _        => state

      private val loadState: F[Msg] =
        for
          currentInternalState <- Async[F].iterateWhile(internalState.get)(_.isEmpty)
          state                <- Async[F].fromOption(currentInternalState, new IllegalStateException("Internal game state is missing"))
          currentNotif         <- notification.get
        yield Msg.GameStateLoaded(state, currentNotif)

      private val sendReady: F[Msg] =
        warClient.ready.as(Msg.LoadGameState)

      private def sendPlayCard(card: Card): F[Msg] =
        warClient.playCard(card).as(Msg.LoadGameState)

      override def subscriptions(state: State): Sub[Msg] =
        Sub.batch(
          state match
            case _: Loading     => Sub.time.everyMs(80, Tick)
            case _: RenderState => Sub.time.everyMs(intervalMs = 300, msg = Msg.LoadGameState),
          Sub.onKeyPress {
            case Key.Char('r')             => Msg.LoadGameState.some
            case Key.Char(' ') | Key.Enter => sendInput(state)
            case Key.Escape                => Msg.Exit.some
            case _                         => None
          }
        )

      private def sendInput(state: State): Option[Msg] =
        state match
          case RenderState(gameState, notif) =>
            gameState match
              case s: Init if s.notReady.contains(playerId)           => Msg.SendReady.some
              case s: PlayerWinTurn if s.notAcked.contains(playerId)  => Msg.SendReady.some
              case s: BattleTurn if s.missingPlays.contains(playerId) => gameState.pickFirstCard(playerId).map(Msg.SendPlayCard(_))
              case s: WarTurn if s.missingPlays.contains(playerId)    => gameState.pickFirstCard(playerId).map(Msg.SendPlayCard(_))
              case _                                                  => None
          case _ => None

      override def view(state: State): Element =
        val elements = state match
          case Loading(tick, _) =>
            val clockSpinner = spinner("Loading...", tick, SpinnerStyle.Clock)
            row(clockSpinner, clockSpinner, clockSpinner)
          case RenderState(s, notif) =>
            s match
              case i: Init          => renderInit(i)
              case bt: BattleTurn   => renderBattleTurn(bt)
              case wt: WarTurn      => renderWarTurn(wt)
              case w: PlayerWinTurn => renderPlayerWinTurn(w)
              case f: Finish        => renderFinish(f)
              case _                => Text(s"current state: ${s.label}")

        appLayout(layout(notificationCard(state.notification), elements))

      private def renderInit(state: Init): Element =
        layout(
          "Waiting for everyone to be ready before starting the game...",
          br,
          section("Players")(kv(playerNames.toSeq.map {
            case (id, name) if state.ready.contains(id) => name.style(Bold) -> "Ready ⚫".color(Blue)
            case (id, name)                             => name.style(Bold) -> "Not ready ⚫".style(Blink).color(Red)
          }*)),
          br,
          if state.notReady.contains(playerId) then Text("Press <space> once you are ready to play...") else empty
        )

      private def renderBattleTurn(state: BattleTurn): Element =
        section(s"Turn ${state.context.turnNumber} : Battle")(
          layout(
            "Everyone play one card and the best one win all the played cards...",
            br,
            if state.missingPlays.nonEmpty then
              section("Waiting cards from players")(kv(playerNames.toSeq.mapFilter {
                case (id, name) if state.missingPlays.contains(id) => (name.style(Bold) -> "Waiting ⚫".style(Blink).color(Red)).some
                case _                                             => None
              }*))
            else empty,
            br,
            section("Played cards")(row(state.playedCards.toSeq.map((id, card) => renderCard(card, title = playerNames(id).some))*).bg(BrightWhite)),
            br,
            if state.missingPlays.contains(playerId) then
              layout(
                section("Card to play")(renderCard(state.pickFirstCard(playerId).get)),
                br,
                "Press <space> to play your card...".style(Blink)
              )
            else "You have already played your card."
          )
        )

      private def renderWarTurn(state: WarTurn): Element =
        val ctx           = state.context
        val round         = state.currentRound
        val missingHidden = state.missingHidden.contains(playerId)
        val action        = if missingHidden then "give a hidden card as prize for the war" else "play your next card"
        val nextCard      = if missingHidden then renderCardBack() else renderCard(ctx.pickFirstCard(playerId).get)
        section(s"Turn ${state.context.turnNumber} : War between ${round.involvedPlayers.map(playerNames).mkString_("[", " | ", "]")}")(
          layout(
            "Every player involved in the war have to play one hidden card and one visible card.",
            "The player with the best visible card win all the played cards.",
            br,
            if state.missingPlays.nonEmpty then
              section("Waiting cards from players")(kv(playerNames.toSeq.mapFilter {
                case (id, name) if state.missingPlays.contains(id) => (name.style(Bold) -> "Waiting ⚫".style(Blink).color(Red)).some
                case _                                             => None
              }*))
            else empty,
            br,
            section("Played cards") {
              val involvedPlayers = state.battles.head.involvedPlayers.toNonEmptyList.toList
              val headers         = "Player" +: state.battles.toList.zipWithIndex.map((_, i) => s"Round ${i + 1}")
              val rows = involvedPlayers.map(id =>
                playerNames(id).style(Bold) +: state.battles.toList.map(round =>
                  row(
                    List(round.hiddenPlayedCards.get(id).as(renderCardBack()), round.fightingCards.get(id).map(renderCard(_))).flatten*
                  ).bg(BrightWhite)
                )
              )
              table(headers, rows).border(Border.None)
            },
            br,
            if state.missingPlays.contains(playerId) then
              layout(
                section("Card to play")(nextCard),
                br,
                s"Press <space> to $action...".style(Blink)
              )
            else "You have already played your card."
          )
        )

      private def renderPlayerWinTurn(state: PlayerWinTurn): Element =
        val ctx        = state.context
        val winnerName = playerNames(state.winnerId)
        val winner     = if state.winnerId == playerId then "You have" else s"Player $winnerName has"
        section(s"Turn ${ctx.turnNumber - 1} : Result")(
          layout(
            s"$winner won turn ${ctx.turnNumber - 1} and won ${state.wonCards.size} cards.",
            br,
            section("Won cards")(row(state.wonCards.toSeq.map(renderCard(_, title = winnerName.some))*).bg(BrightWhite)),
            if state.eliminated.nonEmpty then
              section("Players eliminated during this turn")(kv(state.eliminated.toSeq.map(playerNames(_) -> "Eliminated ❌".color(Red))*))
            else empty,
            "Waiting for everyone to be ready before starting the next turn...",
            br,
            section("Players")(kv(playerNames.toSeq.map {
              case (id, name) if state.acked.contains(id) => name -> "Ready ⚫".color(Blue)
              case (id, name)                             => name -> "Not ready ⚫".style(Blink).color(Red)
            }*)),
            br,
            if state.notAcked.contains(playerId) then "Press <space> once you are ready to play next turn..." else empty
          )
        )

      private def renderFinish(state: Finish): Element =
        val ctx = state.context
        val rankRows = ctx.eliminations.zipWithIndex.map((elimination, index) =>
          Seq(
            s"${index + 2}".style(Bold).color(if index == 0 then Full(253) else if index == 1 then Full(130) else Blue),
            playerNames(elimination.playerId).style(Bold),
            s"${elimination.turn}".style(Bold).color(Red)
          )
        )

        section(s"Game finished after ${ctx.turnNumber - 1} turns.")(
          layout(
            rowTight("The winner is ", playerNames(state.winnerId).style(Bold).color(Full(220)), " !"),
            br,
            table(headers = Seq("Rank", "Player", "Elimination turn"), rows = rankRows).border(Round),
            br,
            Text("Press <escape> to quit the game...")
          )
        )
