package io.tyoras.cards.cli.game.schnapsen

import cats._
import cats.effect.std.Console
import cats.effect.{Async, Clock, ExitCode}
import cats.syntax.all._
import io.chrisdavenport.cats.effect.time.implicits._
import io.chrisdavenport.fuuid.FUUID
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import io.tyoras.cards.cli.{displayCardChoice, displayDeck, lineSeparator}
import io.tyoras.cards.domain.game.schnapsen.{model, _}
import io.tyoras.cards.domain.game.schnapsen.model._

import scala.util.Try

trait SchnapsenCli[F[_]]:
  def run: F[ExitCode]

object SchnapsenCli:

  val banner: String =
    """
      | _____      _
      |/  ___|    | |
      |\ `--.  ___| |__  _ __   __ _ _ __  ___  ___ _ __
      | `--. \/ __| '_ \| '_ \ / _` | '_ \/ __|/ _ \ '_ \
      |/\__/ / (__| | | | | | | (_| | |_) \__ \  __/ | | |
      |\____/ \___|_| |_|_| |_|\__,_| .__/|___/\___|_| |_|
      |                             | |
      |                             |_|                   """.stripMargin

  def apply[F[_]](using F: Async[F], console: Console[F]): SchnapsenCli[F] = new SchnapsenCli[F] {
    given unsafeLogger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLogger[F]

    private val displayIntro: F[Unit] =
      console.println(banner) >>
        console.println(lineSeparator) >>
        console.println("2 players game") >>
        console.println("This game is played using this card deck :") >>
        displayDeck[F](baseDeck) >>
        console.println("At any moment you can use \\q to quit the game or \\r to restart it.") >>
        console.println("")

    private val initGameContext: F[GameContext] =
      def initPlayer(name: String): F[PlayerInfo] = FUUID.randomFUUID[F] map {
        PlayerInfo(_, name)
      }

      for
        human     <- initPlayer("Human")
        ia        <- initPlayer("IA")
        startedAt <- Clock[F].getZonedDateTimeUTC
      yield GameContext(human, ia, startedAt)

    override val run: F[ExitCode] =

      def loop(game: Schnapsen[F]): F[Schnapsen[F]] =
        (for
          state     <- game.currentState
          _         <- renderGameState(state)
          rawInput  <- console.readLine
          gameInput <- parseInput(state, rawInput)
          _         <- game.submitInput(gameInput)
        yield game).handleErrorWith { case InvalidInput =>
          console.println("Your last input is invalid, try again.").as(game)
        }

      for
        _           <- displayIntro
        gameContext <- initGameContext
        game        <- Schnapsen(gameContext)
        exitCode <- game.tailRecM(_.currentState.flatMap {
          case Exit(_) => ExitCode.Success.asRight[Schnapsen[F]].pure[F]
          case _ => loop(game).map(_.asLeft[ExitCode])
        })
      yield exitCode

    private def renderGameState(state: GameState): F[Unit] =
      console.println(lineSeparator) >>
        (state match {
          case _: Init => console.println("Press 'Enter' if you are ready to start the game...")
          case pt: PlayerTurn =>
            val player = pt.currentPlayer
            console.println(s"${player.name} it is your turn, your hand : ${player.hand.mkString(" ")}") >>
              console.println("You can do one of the following actions :") >>
              (pt match {
                case s: EarlyGameForehandTurn =>
                  displayCardChoice[F](s.playableCards) >>
                    console.println(s"\tV : Claim victory") >>
                    Applicative[F].whenA(s.canExchangeTrumpJack) {
                      console.println(s"\tJ : Exchange the trump jack ${s.trumpJack} form your hand with the trump card ${s.round.trumpCard}")
                    } >>
                    s.ongoingMarriage.fold(console.println(s"\tC : Close the talon") >> displayMarriageChoice(s))(m =>
                      console.println(s"You have just meld this two card ${m.king} and ${m.queen}, you must play one of them.")
                    )
                case s: DealerTurn =>
                  console.println(s"${s.round.forehand.name} has played : ${s.forehandCard}") >>
                    console.println("You can play one of the following card(s) from your hand :") >>
                    displayCardChoice[F](s.playableCards)
                case s: LateGameForehandTurn =>
                  displayCardChoice[F](s.playableCards) >>
                    console.println(s"\tV : Claim victory") >>
                    s.ongoingMarriage.fold(displayMarriageChoice(s))(m =>
                      console.println(s"You have just meld this two card ${m.king} and ${m.queen}, you must play one of them.")
                    )
              })
          case s: Finish =>
            for
              _      <- console.println("End of the round.")
              winner <- F.fromEither(s.player(s.outcome.winner))
              loser  <- F.fromEither(s.player(s.outcome.loser))
              _ <-
                console.println(s"The round winner is : ${winner.name} !") >>
                  (s.outcome match {
                    case _: TalonExhausted =>
                      console.println(s"${winner.name} has scored 1 game point by winning the last turn after the talon was exhausted.")
                    case vc: VictoryClaimed =>
                      console.println(
                        s"${winner.name} has scored ${vc.reward} game point(s) ${if vc.successful then "by successfully" else "because the opponent failed at"} claiming victory."
                      )
                  })
              _ <-
                if winner.score <= 0 then
                  console.println("End of the game!") >>
                    console.println(
                      s"${winner.name} has won by reaching a final score of ${winner.score} game point while its opponent ${loser.name} had a final score of ${loser.score} game points."
                    ) >>
                    console.println(s"You can use \\q to quit the game or \\r to restart a new game.")
                else console.println(s"Press 'Enter' when you are ready to start the next round.")
            yield ()
          case _: Exit => F.raiseError(InvalidState) // Exit state is handled exclusively by the main game loop
        })

    private def displayMarriageChoice(state: ForehandTurn): F[Unit] = state.possibleMarriages match
      case Nil => ().pure[F]
      case m :: Nil => console.println(s"\tM : Meld ${m.king} and ${m.queen} for ${m.status.score} points")
      case couples =>
        val choices = couples.zipWithIndex.map { case (m, i) =>
          s"\tM${i + 1} : Meld ${m.king} and ${m.queen} for ${m.status.score} points"
        }
        console.println(s"${choices.mkString("\n")}")

    private def parseInput(state: GameState, rawInput: String): F[Input] =
      rawInput match
        case "\\q" => F.pure(model.End(state.round.forehand.id))
        case "\\r" => F.pure(model.Restart(state.round.forehand.id))
        case _ =>
          state match
            case _: Init => F.pure(model.Start(state.round.forehand.id))
            case s: EarlyGameForehandTurn => parseEarlyGameForehandTurnChoice(s, rawInput)
            case s: DealerTurn => parseCardChoice(s, rawInput)
            case s: LateGameForehandTurn => parseLateGameForehandTurnChoice(s, rawInput)
            case s: Finish =>
              F.fromEither(s.player(s.outcome.winner))
                .map(_.score <= 0)
                .ifM(F.pure(model.End(state.round.forehand.id)), F.pure(model.Start(state.round.forehand.id)))
            case _: Exit => F.raiseError(InvalidState) // Exit state is handled exclusively by the main game loop

    private def parseEarlyGameForehandTurnChoice(state: EarlyGameForehandTurn, rawInput: String): F[Input] =
      rawInput.toLowerCase match
        case "c" => F.pure(model.CloseTalon(state.currentPlayer.id))
        case "j" if state.canExchangeTrumpJack => F.pure(model.ExchangeTrumpJack(state.currentPlayer.id))
        case "v" => F.pure(model.ClaimVictory(state.currentPlayer.id))
        case i if i.startsWith("m") => parseMarriage(state, i)
        case _ => parseCardChoice(state, rawInput)

    private def parseLateGameForehandTurnChoice(state: LateGameForehandTurn, rawInput: String): F[Input] =
      rawInput.toLowerCase match
        case i if i.startsWith("m") => parseMarriage(state, i)
        case "v" => F.pure(model.ClaimVictory(state.currentPlayer.id))
        case _ => parseCardChoice(state, rawInput)

    private def parseCardChoice(state: PlayerTurn, rawInput: String): F[Input] =
      val player = state.currentPlayer
      val playableCards = state.playableCards
      for
        choice <-
          F.fromTry(Try {
            rawInput.toInt
          }).adaptError { case _ => InvalidInput }
        validChoice = choice > 0 && choice <= playableCards.length
        c <-
          if validChoice then
            val card = playableCards(choice - 1)
            Logger[F].debug(s"Player ${player.name} has played $card") >>
              model.PlayCard(player.id, card).pure[F]
          else F.raiseError[Input](InvalidInput)
      yield c

    private def parseMarriage(state: ForehandTurn, rawInput: String): F[Input] =
      rawInput.toCharArray match
        case Array('m') => parseMarriageChoice(state, none)
        case Array('m', n) => parseMarriageChoice(state, n.some)
        case _ => F.raiseError[Input](InvalidInput)

    private def parseMarriageChoice(state: ForehandTurn, rawInput: Option[Char]): F[Input] =
      val player = state.currentPlayer
      val validMarriages = state.possibleMarriages
      for
        choice <-
          F.fromTry(Try {
            rawInput.map(_.asDigit).getOrElse(1)
          }).adaptError { case _ => InvalidInput }
        validChoice = choice > 0 && choice <= validMarriages.length
        c <-
          if validChoice then
            val m = validMarriages(choice - 1)
            Logger[F].debug(s"Player ${player.name} has meld ${m.king} and ${m.queen} for ${m.status.score} points") >>
              model.Meld(player.id, m.king.suit).pure[F]
          else F.raiseError[Input](InvalidInput)
      yield c
  }
