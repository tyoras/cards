package io.tyoras.cards.cli.remote.game.ers

import cats.effect.{Async, Ref}
import cats.effect.std.Dispatcher
import io.tyoras.cards.cli.remote.client.{ChatClient, ERsClient}
import io.tyoras.cards.cli.remote.game.ers.ERsTUI.{Msg, State}
import io.tyoras.cards.cli.tui.*
import io.tyoras.cards.domain.game.ers.model.{PlayerGameState, PlayerId}
import io.tyoras.cards.domain.game.ers.model.PlayerGameState.*
import layoutz.*
import layoutz.Key.{Char as _, *}
import cats.syntax.all.*
import io.tyoras.cards.cli.remote.game.ers.ERsTUI.Msg.Tick
import io.tyoras.cards.cli.remote.game.ers.ERsTUI.State.*
import io.tyoras.cards.cli.tui.TUI.Message
import io.tyoras.cards.cli.tui.TUI.Message.Notification
import io.tyoras.cards.cli.tui.TUI.Message.Notification.Error
import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.game.ers.penalty
import layoutz.Border.Round
import layoutz.Color.{Blue, BrightWhite, Full, Red}
import layoutz.Style.{Blink, Bold}

trait ERsTUI[F[_]] extends TUI[F, State, Msg]
object ERsTUI:
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
    case SendSlap
    case SendPlayCard
    case SendDiscard(card: Card)
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
      ersClient: ERsClient[F],
      chatClient: ChatClient.ConnectedClient[F]
  ): Dispatcher[F] => ERsTUI[F] = d =>
    new ERsTUI[F]:
      override protected def pageBanner: String = banner

      override protected def dispatcher: Dispatcher[F] = d

      override def init: (State, Cmd[Msg]) =
        Loading() -> Cmd.batch(
          Cmd.setTitle(s"Egyptian ratscrew game - ${ersClient.gameId}"),
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
                  case Msg.SendReady    => state -> runEffectCmd(sendReady)(e => Msg.DisplayNotif(Notification.Error(s"Failed to send ready input: $e")))
                  case Msg.SendSlap     => state -> runEffectCmd(sendSlap)(e => Msg.DisplayNotif(Notification.Error(s"Failed to send slap input: $e")))
                  case Msg.SendPlayCard =>
                    state -> runEffectCmd(sendPlayCard)(e => Msg.DisplayNotif(Notification.Error(s"Failed to send play card input: $e")))
                  case Msg.SendDiscard(card) =>
                    state -> runEffectCmd(sendDiscard(card))(e => Msg.DisplayNotif(Notification.Error(s"Failed to send discard card input: $e")))
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
        ersClient.ready.as(Msg.LoadGameState)

      private val sendSlap: F[Msg] =
        ersClient.slap.as(Msg.LoadGameState)

      private val sendPlayCard: F[Msg] =
        ersClient.playCard.as(Msg.LoadGameState)

      private def sendDiscard(card: Card): F[Msg] =
        ersClient.discard(card).as(Msg.LoadGameState)

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
            case Key.Char('s') if !state.chatActive                             => sendSlapInput(state)
            case Key.Escape                                                     => Msg.Exit.some
            case Key.Char(c) if state.chatActive                                => Msg.TypeChar(c).some
            case Key.Backspace if state.chatActive && state.currentMsg.nonEmpty => Msg.Backspace.some
            case Key.Enter if state.chatActive && state.currentMsg.nonEmpty     => Msg.SubmitChatMessage(state.currentMsg).some
            case _                                                              => None
          }
        )

      private val sendInput: State => Option[Msg] =
        case RenderState(gameState, notif, _, _, _) =>
          gameState match
            case s: Init if s.notReady.contains(playerId)          => Msg.SendReady.some
            case s: PlayerWinTurn if s.notAcked.contains(playerId) => Msg.SendReady.some
            case s: PlayerTurn if s.waitedPlayer == playerId       => Msg.SendPlayCard.some
            case s: ChallengeTurn if s.challenged == playerId      => Msg.SendPlayCard.some
            case s: SlapPenalty if s.penalizedPlayer == playerId   => s.handTopCard.filter(_ => s.remainingCardsToDiscard > 0).map(Msg.SendDiscard(_))
            case s: SlapPenalty if s.notAcked.contains(playerId)   => Msg.SendReady.some
            case _                                                 => None
        case _ => None

      private val sendSlapInput: State => Option[Msg] =
        case RenderState(gameState, notif, _, _, _) =>
          gameState match
            case s: (PlayerTurn | ChallengeTurn) => Msg.SendSlap.some
            case _                               => None
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
              case i: Init           => renderInit(i, chatWidth)
              case pt: PlayerTurn    => renderPlayerTurn(pt, chatWidth)
              case ct: ChallengeTurn => renderChallengeTurn(ct, chatWidth)
              case sp: SlapPenalty   => renderSlapPenalty(sp, chatWidth)
              case w: PlayerWinTurn  => renderPlayerWinTurn(w, chatWidth)
              case f: Finish         => renderFinish(f)
              case _                 => Text(s"current state: ${s.label}") -> List.empty

        appLayout(columns(box("Game")(leftCol), layout(rightColElems :+ chat :+ "Use <tab> to switch between game and chat."*)))

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
                case (id, name) if state.ready.contains(id) => rowTight("●".color(Blue), s" $name".style(Bold))             -> "Ready".color(Blue)
                case (id, name)                             => rowTight("●".color(Red).style(Blink), s" $name".style(Bold)) -> "Not ready".color(Red)
              }*)
            )
          ).border(Round),
          br
        )

      private def renderPlayerTurn(state: PlayerTurn, chatWidth: Int): (Element, List[Element]) =
        val playCardMsg = "Press <space> to play your card..."
        section(s"Turn ${state.turn} : Battle")(
          layout(
            "Everyone play one card at a time on top of the pile until a face card is played...",
            br,
            columns(
              layout(
                section("Your hand")(
                  layout(
                    renderCardCount("Hand size", state.handSize),
                    renderCardBack(),
                    br,
                    if state.waitedPlayer == playerId then playCardMsg.style(Blink) else space(playCardMsg.length)
                  )
                )
              ),

              renderPile(state.pileTopCard, state.pileSize, slapAllowed = true)
            )
          )
        ) -> List(
          box("Player status")(
            layout(
              "=== Waiting card from player ===".leftAlign(chatWidth),
              kv(playerNames.toSeq.mapFilter {
                case (id, name) if state.waitedPlayer == id =>
                  (rowTight("●".style(Blink).color(Red), s" $name".style(Bold)) -> "Waiting".color(Red)).some
                case _ => None
              }*)
            )
          ).border(Round),
          br
        )

      private def renderChallengeTurn(state: ChallengeTurn, chatWidth: Int): (Element, List[Element]) =
        val playCardMsg = "Press <space> to play your card..."
        section(s"Turn ${state.turn} : Challenge")(
          layout(
            s"By playing a face card ${playerNames(state.challenger)} has challenged ${playerNames(state.challenged)} to also play a face card, ${state.remainingTries}/${state.totalTries} remaining tries.",
            br,
            columns(
              section("Your hand")(
                layout(
                  renderCardCount("Hand size", state.handSize),
                  renderCardBack(),
                  br,
                  if state.challenged == playerId then playCardMsg.style(Blink) else space(playCardMsg.length)
                )
              ),
              renderPile(state.pileTopCard.some, state.pileSize, slapAllowed = true)
            )
          )
        ) -> List(
          box("Player status")(
            layout(
              "=== Waiting card from player ===".leftAlign(chatWidth),
              kv(playerNames.toSeq.mapFilter {
                case (id, name) if state.challenged == id =>
                  (rowTight("●".style(Blink).color(Red), s" $name".style(Bold)) -> "Waiting".color(Red)).some
                case _ => None
              }*)
            )
          ).border(Round),
          br
        )

      private def renderSlapPenalty(state: SlapPenalty, chatWidth: Int): (Element, List[Element]) = {
        val player         = if state.penalizedPlayer == playerId then "You have" else s"${playerNames(state.penalizedPlayer)} has"
        val shouldDiscard  = state.penalizedPlayer == playerId && state.remainingCardsToDiscard > 0 && state.handSize > 0
        val discardCardMsg = "Press <space> to discard your card..."
        section(s"Turn ${state.turn} : Slap penalty")(
          layout(
            s"$player slapped the pile but in an invalid situation so $player to discard $penalty card(s).",
            br,
            columns(
              section("Your hand")(
                layout(
                  renderCardCount("Hand size", state.handSize),
                  if shouldDiscard then state.handTopCard.fold(renderCardBack())(renderCard(_)) else renderCardBack(),
                  br,
                  if shouldDiscard then discardCardMsg.style(Blink)
                  else if state.notAcked.contains(playerId) then "Press <space> once you are ready to resume playing..."
                  else space(discardCardMsg.length)
                )
              ),
              renderPile(state.pileTopCard.some, state.pileSize, slapAllowed = false)
            )
          )
        ) -> List(
          box("Player status")(
            layout(
              (if state.remainingCardsToDiscard > 0 then
                 "=== Waiting card from player ===".leftAlign(chatWidth) ::
                   (kv(playerNames.toSeq.mapFilter {
                     case (id, name) if state.penalizedPlayer == id =>
                       (rowTight("●".style(Blink).color(Red), s" $name".style(Bold)) -> "Waiting".color(Red)).some
                     case _ => None
                   }*) :: br :: Nil)
               else Nil) ++
                ("=== Ready to play next turn ===".leftAlign(chatWidth) ::
                  kv(playerNames.toSeq.map {
                    case (id, name) if state.acked.contains(id) => rowTight("●".color(Blue), s" $name".style(Bold)) -> "Ready".color(Blue)
                    case (id, name) if state.penalizedPlayer == id && state.remainingCardsToDiscard == 0 =>
                      rowTight("●".color(Blue), s" $name".style(Bold)) -> "Ready".color(Blue)
                    case (id, name) => rowTight("●".color(Red).style(Blink), s" $name".style(Bold)) -> "Not ready".color(Red)
                  }*) :: Nil)*
            )
          ).border(Round),
          br
        )
      }

      private def renderCardCount(title: String, count: Int): Element =
        statusCard(title, if count < 2 then s"$count CARD" else s"$count CARDS").border(Border.Double)

      private def renderPile(pileTopCard: Option[Card], pileSize: Int, slapAllowed: Boolean): Element = {
        val slapMsg = "Press [S] to slap the pile !"
        section("Pile")(
          layout(
            renderCardCount("Pile size", pileSize),
            pileTopCard.fold(renderCardBack(title = "Empty".some))(topCard => renderCard(topCard, title = "last played".some)).colorBg(BrightWhite),
            br,
            if pileSize > 0 && slapAllowed then slapMsg else space(slapMsg.length)
          )
        )
      }

      private def renderPlayerWinTurn(state: PlayerWinTurn, chatWidth: Int): (Element, List[Element]) =
        val turn       = state.turn
        val winnerName = playerNames(state.winnerId)
        val winner     = if state.winnerId == playerId then "You have" else s"Player $winnerName has"
        section(s"Turn $turn : Result")(
          layout(
            s"$winner won turn $turn and won ${state.wonCards.size} cards.",
            br,
            section("Won cards")(renderCards(state.wonCards.toList, 4).colorBg(BrightWhite)),
            if state.eliminated.nonEmpty then
              section("Players eliminated during this turn")(kv(state.eliminated.toSeq.map(playerNames(_) -> "Eliminated ✗".color(Red))*))
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
                case (id, name) if state.acked.contains(id) => rowTight("●".color(Blue), s" $name".style(Bold))             -> "Ready".color(Blue)
                case (id, name)                             => rowTight("●".color(Red).style(Blink), s" $name".style(Bold)) -> "Not ready".color(Red)
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
