package io.tyoras.cards.cli.remote.game.war

import cats.effect.{Async, Ref}
import cats.effect.std.Dispatcher
import io.tyoras.cards.cli.remote.client.{ChatClient, WarClient}
import io.tyoras.cards.cli.remote.game.war.WarTUI.{Msg, State}
import io.tyoras.cards.cli.tui.*
import io.tyoras.cards.domain.game.war.model.{PlayerGameState, PlayerId}
import io.tyoras.cards.domain.game.war.model.PlayerGameState.*
import layoutz.*
import layoutz.Key.{Char as _, *}
import cats.syntax.all.*
import io.tyoras.cards.cli.remote.game.war.WarTUI.Msg.Tick
import io.tyoras.cards.cli.remote.game.war.WarTUI.State.*
import io.tyoras.cards.cli.tui.TUI.Message
import io.tyoras.cards.cli.tui.TUI.Message.Notification
import io.tyoras.cards.cli.tui.TUI.Message.Notification.Error
import io.tyoras.cards.domain.card.Card
import layoutz.Border.Round
import layoutz.Color.{Blue, BrightWhite, Full, Red}
import layoutz.Style.{Blink, Bold}

trait WarTUI[F[_]] extends TUI[F, State, Msg]

object WarTUI:
  enum State(val messages: List[Message], val currentMsg: String, val chatActive: Boolean, val tick: Int):
    case Loading(tack: Int = 0, msgs: List[Message] = List.empty, crtMsg: String = "", active: Boolean = false) extends State(msgs, crtMsg, active, tack)
    case RenderState(gameState: PlayerGameState, msgs: List[Message] = List.empty, tack: Int, crtMsg: String = "", writingInChat: Boolean = false)
        extends State(msgs, crtMsg, writingInChat, tack)

    def incrementTick(inc: Int = 1): State =
      this match
        case l: Loading      => l.copy(tack = tick + inc)
        case rs: RenderState => rs.copy(tack = tick + inc)

    def addNotif(notif: Notification): State =
      this match
        case l: Loading      => l.copy(msgs = messages :+ notif)
        case rs: RenderState => rs.copy(msgs = messages :+ notif)

    def typeChar(c: Char): State =
      this match
        case l: Loading      => l.copy(crtMsg = currentMsg + c)
        case rs: RenderState => rs.copy(crtMsg = currentMsg + c)

    def removeChar(): State =
      this match
        case l: Loading      => l.copy(crtMsg = currentMsg.dropRight(1))
        case rs: RenderState => rs.copy(crtMsg = currentMsg.dropRight(1))

    def emptyCurrentMsg(): State =
      this match
        case l: Loading      => l.copy(crtMsg = "")
        case rs: RenderState => rs.copy(crtMsg = "")

  enum Msg:
    case DisplayNotif(notif: Notification)
    case MoveUp
    case MoveDown
    case LoadGameState
    case GameStateLoaded(gameState: PlayerGameState, messages: List[Message])
    case SendReady
    case SendPlayCard(card: Card)
    case Tick
    case ToggleChatFocus
    case TypeChar(c: Char)
    case Backspace
    case SubmitChatMessage(msg: String)
    case Exit

  def make[F[_] : Async](
      banner: String,
      playerId: PlayerId,
      playerNames: Map[PlayerId, String],
      internalState: Ref[F, Option[PlayerGameState]],
      messages: Ref[F, List[Message]],
      warClient: WarClient[F],
      chatClient: ChatClient.ConnectedClient[F]
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

      override def update(msg: Msg, state: State): (State, Cmd[Msg]) = {
        msg match
          case Msg.Tick                   => state.incrementTick() -> Cmd.none
          case Msg.DisplayNotif(notif)    => state.addNotif(notif) -> Cmd.none
          case Msg.TypeChar(c)            => state.typeChar(c)     -> Cmd.none
          case Msg.Backspace              => state.removeChar()    -> Cmd.none
          case Msg.SubmitChatMessage(msg) =>
            state.emptyCurrentMsg() -> runEffectCmd(submitChatMessage(state.currentMsg))(_ =>
              Msg.DisplayNotif(Notification.Error(s"Failed to send chat message: ${state.currentMsg}"))
            )
          case Msg.Exit => state -> Cmd.exit
          case _        =>
            state match
              case state @ Loading(tick, msgs, crtMsg, writingInChat) =>
                msg match
                  case Msg.GameStateLoaded(s, msgs) => RenderState(s, msgs, tick, crtMsg, writingInChat) -> Cmd.none
                  case _                            => state                                             -> Cmd.none
              case state @ RenderState(gameState, msgs, tick, crtMsg, writingInChat) =>
                msg match
                  case Msg.LoadGameState =>
                    Loading(crtMsg = crtMsg, active = writingInChat) -> runEffectCmd(loadState)(e =>
                      Msg.DisplayNotif(Notification.Error(s"Failed to load game state: $e"))
                    )
                  case Msg.SendReady          => state -> runEffectCmd(sendReady)(e => Msg.DisplayNotif(Notification.Error(s"Failed to send ready input: $e")))
                  case Msg.SendPlayCard(card) =>
                    state -> runEffectCmd(sendPlayCard(card))(e => Msg.DisplayNotif(Notification.Error(s"Failed to send play card input: $e")))
                  case Msg.ToggleChatFocus => state.copy(writingInChat = !writingInChat)
                  case _                   => state
      }

      private val loadState: F[Msg] =
        for
          currentInternalState <- Async[F].iterateWhile(internalState.get)(_.isEmpty)
          state                <- Async[F].fromOption(currentInternalState, new IllegalStateException("Internal game state is missing"))
          currentMessages      <- messages.get
        yield Msg.GameStateLoaded(state, currentMessages)

      private val sendReady: F[Msg] =
        warClient.ready.as(Msg.LoadGameState)

      private def sendPlayCard(card: Card): F[Msg] =
        warClient.playCard(card).as(Msg.LoadGameState)

      private def submitChatMessage(msg: String): F[Msg] =
        chatClient.chat(msg).as(Msg.LoadGameState)

      override def subscriptions(state: State): Sub[Msg] =
        Sub.batch(
          Sub.time.everyMs(350, Tick),
          state match
            case _: Loading     => Sub.none
            case _: RenderState => Sub.time.everyMs(intervalMs = 300, msg = Msg.LoadGameState),
          Sub.onKeyPress {
            case Key.Char('r') if !state.chatActive                             => Msg.LoadGameState.some
            case Key.Tab                                                        => Msg.ToggleChatFocus.some
            case Key.Char(' ') | Key.Enter if !state.chatActive                 => sendInput(state)
            case Key.Escape                                                     => Msg.Exit.some
            case Key.Char(c) if state.chatActive                                => Msg.TypeChar(c).some
            case Key.Backspace if state.chatActive && state.currentMsg.nonEmpty => Msg.Backspace.some
            case Key.Enter if state.chatActive && state.currentMsg.nonEmpty     => Msg.SubmitChatMessage(state.currentMsg).some
            case _                                                              => None
          }
        )

      private def sendInput(state: State): Option[Msg] =
        state match
          case RenderState(gameState, notif, _, _, _) =>
            gameState match
              case s: Init if s.notReady.contains(playerId)           => Msg.SendReady.some
              case s: PlayerWinTurn if s.notAcked.contains(playerId)  => Msg.SendReady.some
              case s: BattleTurn if s.missingPlays.contains(playerId) => s.pickFirstCard.map(Msg.SendPlayCard(_))
              case s: WarTurn if s.missingPlays.contains(playerId)    => s.pickFirstCard.map(Msg.SendPlayCard(_))
              case _                                                  => None
          case _ => None

      override def view(state: State): Element =
        val chat                     = chatElement(state.messages, state.currentMsg, state.chatActive)
        val chatWidth                = chat.width - 4 // minus border*2 + space*2
        val (leftCol, rightColElems) = state match
          case Loading(tick, _, _, _) =>
            val clockSpinner = spinner("Loading...", tick, SpinnerStyle.Clock)
            row(clockSpinner, clockSpinner, clockSpinner) -> List.empty
          case RenderState(s, notif, tick, _, _) =>
            s match
              case i: Init          => renderInit(i, chatWidth)
              case bt: BattleTurn   => renderBattleTurn(bt, chatWidth)
              case wt: WarTurn      => renderWarTurn(wt, chatWidth)
              case w: PlayerWinTurn => renderPlayerWinTurn(w, chatWidth)
              case f: Finish        => renderFinish(f)
              case _                => Text(s"current state: ${s.label}") -> List.empty

        appLayout(columns(leftCol, layout(rightColElems :+ chat :+ "Use <tab> to switch between game and chat."*)))

      private def renderInit(state: Init, chatWidth: Int): (Element, List[Element]) =
        layout(
          "Waiting for everyone to be ready before starting the game...",
          br,
          if state.notReady.contains(playerId) then Text("Press <space> once you are ready to play...") else empty
        ) -> List(
          box("Player status")(
            layout(
              "=== Ready to start ===".leftAlign(chatWidth),
              kv(playerNames.toSeq.map {
                case (id, name) if state.ready.contains(id) => rowTight("⚫".color(Blue), s" $name".style(Bold))             -> "Ready".color(Blue)
                case (id, name)                             => rowTight("⚫".color(Red).style(Blink), s" $name".style(Bold)) -> "Not ready".color(Red)
              }*)
            )
          ).border(Round),
          br
        )

      private def renderBattleTurn(state: BattleTurn, chatWidth: Int): (Element, List[Element]) =
        section(s"Turn ${state.turn} : Battle")(
          layout(
            "Everyone play one card and the best one win all the played cards...",
            br,
            section("Played cards")(row(state.playedCards.toSeq.map((id, card) => renderCard(card, title = playerNames(id).some))*).colorBg(BrightWhite)),
            br,
            if state.missingPlays.contains(playerId) then
              layout(
                section("Card to play")(renderCard(state.pickFirstCard.get)),
                br,
                "Press <space> to play your card...".style(Blink)
              )
            else "You have already played your card."
          )
        ) -> List(
          box("Player status")(
            layout(
              "=== Waiting cards from players ===".leftAlign(chatWidth),
              kv(playerNames.toSeq.mapFilter {
                case (id, name) if state.missingPlays.contains(id) =>
                  (rowTight("⚫".style(Blink).color(Red), s" $name".style(Bold)) -> "Waiting".color(Red)).some
                case _ => None
              }*)
            )
          ).border(Round),
          br
        )

      private def renderWarTurn(state: WarTurn, chatWidth: Int): (Element, List[Element]) =
        val round         = state.currentRound
        val missingHidden = state.missingHidden.contains(playerId)
        val action        = if missingHidden then "give a hidden card as prize for the war" else "play your next card"
        val nextCard      = if missingHidden then renderCardBack() else renderCard(state.pickFirstCard.get)
        section(s"Turn ${state.turn} : War between ${round.involvedPlayers.map(playerNames).mkString_("[", " | ", "]")}")(
          layout(
            "Every player involved in the war have to play one hidden card and one visible card.",
            "The player with the best visible card win all the played cards.",
            br,
            section("Played cards") {
              val involvedPlayers = state.battles.head.involvedPlayers.toNonEmptyList.toList
              val headers         = "Player" +: state.battles.toList.zipWithIndex.map((_, i) => s"Round ${i + 1}")
              val rows            = involvedPlayers.map(id =>
                playerNames(id).style(Bold) +: state.battles.toList.map(round =>
                  row(
                    List(round.hiddenPlayedCards.get(id).as(renderCardBack()), round.fightingCards.get(id).map(renderCard(_))).flatten*
                  ).colorBg(BrightWhite)
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
        ) -> List(
          box("Player status")(
            layout(
              "=== Waiting cards from players ===".leftAlign(chatWidth),
              kv(playerNames.toSeq.mapFilter {
                case (id, name) if state.missingPlays.contains(id) =>
                  (rowTight("⚫".style(Blink).color(Red), s" $name".style(Bold)) -> "Waiting".color(Red)).some
                case _ => None
              }*)
            )
          ).border(Round),
          br
        )

      private def renderPlayerWinTurn(state: PlayerWinTurn, chatWidth: Int): (Element, List[Element]) =
        val turn       = state.turn - 1
        val winnerName = playerNames(state.winnerId)
        val winner     = if state.winnerId == playerId then "You have" else s"Player $winnerName has"
        section(s"Turn $turn : Result")(
          layout(
            s"$winner won turn $turn and won ${state.wonCards.size} cards.",
            br,
            section("Won cards")(row(state.wonCards.toSeq.map(renderCard(_, title = winnerName.some))*).colorBg(BrightWhite)),
            if state.eliminated.nonEmpty then
              section("Players eliminated during this turn")(kv(state.eliminated.toSeq.map(playerNames(_) -> "Eliminated ❌".color(Red))*))
            else empty,
            "Waiting for everyone to be ready before starting the next turn...",
            br,
            if state.notAcked.contains(playerId) then "Press <space> once you are ready to play next turn..." else empty
          )
        ) -> List(
          box("Player status")(
            layout(
              "=== Ready to play next turn ===".leftAlign(chatWidth),
              kv(playerNames.toSeq.map {
                case (id, name) if state.acked.contains(id) => rowTight("⚫".color(Blue), s" $name".style(Bold))             -> "Ready".color(Blue)
                case (id, name)                             => rowTight("⚫".color(Red).style(Blink), s" $name".style(Bold)) -> "Not ready".color(Red)
              }*)
            )
          ).border(Round),
          br
        )

      private def renderFinish(state: Finish): (Element, List[Element]) =
        val rankRows = state.eliminations.zipWithIndex.map((elimination, index) =>
          Seq(
            s"${index + 2}".style(Bold).color(if index == 0 then Full(253) else if index == 1 then Full(130) else Blue),
            playerNames(elimination.playerId).style(Bold),
            s"${elimination.turn}".style(Bold).color(Red)
          )
        )

        section(s"Game finished after ${state.turn - 1} turns.")(
          layout(
            rowTight("The winner is ", playerNames(state.winnerId).style(Bold).color(Full(220)), " !"),
            br,
            table(headers = Seq("Rank", "Player", "Elimination turn"), rows = rankRows).border(Round),
            br,
            Text("Press <escape> to quit the game...")
          )
        ) -> List.empty
