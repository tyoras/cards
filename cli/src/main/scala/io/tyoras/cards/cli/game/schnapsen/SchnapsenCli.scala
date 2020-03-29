package io.tyoras.cards.cli.game.schnapsen

import cats._
import cats.effect.{Console, ExitCode, Sync}
import cats.implicits._
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.{Logger, SelfAwareStructuredLogger}
import io.tyoras.cards.cli.{displayCardChoice, displayDeck, lineSeparator}
import io.tyoras.cards.game.schnapsen._
import io.tyoras.cards.game.schnapsen.model._

import scala.util.Try

trait SchnapsenCli[F[_]] {
  def run: F[ExitCode]
}

object SchnapsenCli {

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

  def apply[F[_]](implicit F: Sync[F], console: Console[F]): SchnapsenCli[F] = new SchnapsenCli[F] {
    implicit val unsafeLogger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLogger[F]

    private val displayIntro: F[Unit] =
      console.putStrLn(banner) >>
        console.putStrLn(lineSeparator) >>
        console.putStrLn("2 players game") >>
        console.putStrLn("This game is played using this card deck :") >>
        displayDeck[F](baseDeck) >>
        console.putStrLn("At any moment you can use \\q to quit the game or \\r to restart it.") >>
        console.putStrLn("")

    override val run: F[ExitCode] = {
      val schnapsen = Schnapsen[F]

      def transition(state: GameState): F[GameState] = {
        (for {
          _                <- renderGameState(state)
          rawInput         <- console.readLn
          gameInput        <- parseInput(state, rawInput)
          updatedGameState <- schnapsen.submitInput(state, gameInput)
        } yield updatedGameState).handleErrorWith {
          case InvalidInput => console.putStrLn("Your last input is invalid, try again.") >> state.pure[F]
        }.tailRecM(_.map {
          case s: Exit => Right(s)
          case s => Left(transition(s))
        })
      }

      for {
        _     <- displayIntro
        human <- initPlayer("Human")
        ia    <- initPlayer("IA")
        init  <- schnapsen.initGame(human, ia)
        _     <- transition(init)
      } yield ExitCode.Success
    }

    private def initPlayer(name: String): F[Player] = FUUID.randomFUUID[F] map {
      Player(_, name)
    }

    private def renderGameState(state: GameState): F[Unit] =
      console.putStrLn(lineSeparator) >>
        (state match {
          case _: Init => console.putStrLn("Press 'Enter' if you are ready to start the game...")
          case pt: PlayerTurn =>
            val player = pt.currentPlayer
            console.putStrLn(s"${player.name} it is your turn, your hand : ${player.hand.mkString(" ")}") >>
              console.putStrLn("You can do one of the following actions :") >>
              (pt match {
                case s: EarlyGameForehandTurn =>
                  displayCardChoice[F](s.playableCards) >>
                    console.putStrLn(s"\tV : Claim victory") >>
                    Applicative[F].whenA(s.canExchangeTrumpJack) {
                      console.putStrLn(s"\tJ : Exchange the trump jack ${s.trumpJack} form your hand with the trump card ${s.game.trumpCard}")
                    } >>
                    s.ongoingMarriage.fold(console.putStrLn(s"\tC : Close the talon") >> displayMarriageChoice(s))(m =>
                      console.putStrLn(s"You have just meld this two card ${m.king} and ${m.queen}, you must play one of them.")
                    )
                case s: DealerTurn =>
                  console.putStrLn(s"${s.game.forehand.name} has played : ${s.forehandCard}") >>
                    console.putStrLn("You can play one of the following card(s) from your hand :") >>
                    displayCardChoice[F](s.playableCards)
                case s: LateGameForehandTurn =>
                  displayCardChoice[F](s.playableCards) >>
                    console.putStrLn(s"\tV : Claim victory") >>
                    s.ongoingMarriage.fold(displayMarriageChoice(s))(m =>
                      console.putStrLn(s"You have just meld this two card ${m.king} and ${m.queen}, you must play one of them.")
                    )
              })
          case s: Finish =>
            console.putStrLn("End of the game.") >>
              console.putStrLn(s"And the winner is : ${s.winner.name} !") >>
              console.putStrLn(s"You can use \\q to quit the game or \\r to restart a new game.")
        })

    private def displayMarriageChoice(state: ForehandTurn): F[Unit] = state.possibleMarriages match {
      case Nil => ().pure[F]
      case m :: Nil => console.putStrLn(s"\tM : Meld ${m.king} and ${m.queen} for ${m.status.score} points")
      case couples =>
        val choices = couples.zipWithIndex.map {
          case (m, i) => s"\tM${i + 1} : Meld ${m.king} and ${m.queen} for ${m.status.score} points"
        }
        console.putStrLn(s"${choices.mkString("\n")}")
    }

    private def parseInput(state: GameState, rawInput: String): F[Input] = {
      rawInput match {
        case "\\q" => F.pure(End(state.game.forehand.id))
        case "\\r" => F.pure(Restart(state.game.forehand.id))
        case _ =>
          state match {
            case _: Init => F.pure(Start(state.game.forehand.id))
            case s: EarlyGameForehandTurn => parseEarlyGameForehandTurnChoice(s, rawInput)
            case s: DealerTurn => parseCardChoice(s, rawInput)
            case s: LateGameForehandTurn => parseLateGameForehandTurnChoice(s, rawInput)
            case _: Finish => F.pure(End(state.game.forehand.id))
          }
      }
    }

    private def parseEarlyGameForehandTurnChoice(state: EarlyGameForehandTurn, rawInput: String): F[Input] =
      rawInput.toLowerCase match {
        case "c" => F.pure(CloseTalon(state.currentPlayer.id))
        case "j" if state.canExchangeTrumpJack => F.pure(ExchangeTrumpJack(state.currentPlayer.id))
        case "v" => F.pure(ClaimVictory(state.currentPlayer.id))
        case i if i.startsWith("m") => parseMarriage(state, i)
        case _ => parseCardChoice(state, rawInput)
      }

    private def parseLateGameForehandTurnChoice(state: LateGameForehandTurn, rawInput: String): F[Input] =
      rawInput.toLowerCase match {
        case i if i.startsWith("m") => parseMarriage(state, i)
        case "v" => F.pure(ClaimVictory(state.currentPlayer.id))
        case _ => parseCardChoice(state, rawInput)
      }

    private def parseCardChoice(state: PlayerTurn, rawInput: String): F[Input] = {
      val player = state.currentPlayer
      val playableCards = state.playableCards
      for {
        choice <- F
          .fromTry(Try {
            rawInput.toInt
          })
          .adaptError { case _ => InvalidInput }
        validChoice = choice > 0 && choice <= playableCards.length
        c <- if (validChoice) {
          val card = playableCards(choice - 1)
          Logger[F].debug(s"Player ${player.name} has played $card") >>
            PlayCard(player.id, card).pure[F]
        } else {
          F.raiseError[Input](InvalidInput)
        }
      } yield c
    }

    private def parseMarriage(state: ForehandTurn, rawInput: String): F[Input] =
      rawInput.toCharArray match {
        case Array('m') => parseMarriageChoice(state, none)
        case Array('m', n) => parseMarriageChoice(state, n.some)
        case _ => F.raiseError[Input](InvalidInput)
      }

    private def parseMarriageChoice(state: ForehandTurn, rawInput: Option[Char]): F[Input] = {
      val player = state.currentPlayer
      val validMarriages = state.possibleMarriages
      for {
        choice <- F
          .fromTry(Try {
            rawInput.map(_.asDigit).getOrElse(1)
          })
          .adaptError { case _ => InvalidInput }
        validChoice = choice > 0 && choice <= validMarriages.length
        c <- if (validChoice) {
          val m = validMarriages(choice - 1)
          Logger[F].debug(s"Player ${player.name} has meld ${m.king} and ${m.queen} for ${m.status.score} points") >>
            Meld(player.id, m.king.suit).pure[F]
        } else {
          F.raiseError[Input](InvalidInput)
        }
      } yield c
    }
  }
}
