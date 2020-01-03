package io.tyoras.cards.cli.game

import java.util.UUID

import cats._
import cats.effect.{Console, ExitCode, Sync}
import cats.implicits._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.{Logger, SelfAwareStructuredLogger}
import io.tyoras.cards.cli.{InvalidInput, displayCardChoice, displayDeck, lineSeparator}
import io.tyoras.cards.game.schnapsen.Schnapsen.{initGame, submitInput}
import io.tyoras.cards.game.schnapsen._

import scala.util.Try

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

  implicit def unsafeLogger[F[_] : Sync]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLogger[F]

  def run[F[_] : Sync](implicit console: Console[F]): F[ExitCode] = {

    def transition(state: GameState): F[GameState] = {
      (for {
        _                <- renderGameState[F](state)
        rawInput         <- console.readLn
        gameInput        <- parseInput[F](state, rawInput)
        updatedGameState <- submitInput[F](state, gameInput)
      } yield updatedGameState).handleErrorWith {
        case InvalidInput => console.putStrLn("Your last input is invalid, try again.") >> state.pure[F]
      }.tailRecM(_.map {
        case s: Exit => Right(s)
        case s => Left(transition(s))
      })
    }

    for {
      _     <- displayIntro[F]
      human <- initPlayer("Human")
      ia    <- initPlayer("IA")
      init  <- initGame(human, ia)
      _     <- transition(init)
    } yield ExitCode.Success
  }

  private def displayIntro[F[_] : Monad](implicit console: Console[F]): F[Unit] =
    console.putStrLn(banner) >>
      console.putStrLn(lineSeparator) >>
      console.putStrLn("2 players game") >>
      console.putStrLn("This game is played using this card deck :") >>
      displayDeck[F](baseDeck) >>
      console.putStrLn("At any moment you can use \\q to quit the game or \\r to restart it.") >>
      console.putStrLn("")

  private def initPlayer[F[_] : Sync](name: String): F[Player] = Sync[F].delay {
    Player(UUID.randomUUID(), name)
  }

  private def renderGameState[F[_] : Sync](state: GameState)(implicit console: Console[F]): F[Unit] =
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
                  Applicative[F].whenA(s.canExchangeTrumpJack) {
                    console.putStrLn(s"\tJ : Exchange the trump jack ${s.trumpJack} form your hand with the trump card ${s.game.trumpCard}")
                  } >>
                  console.putStrLn(s"\tC : Close the talon")
              case s: EarlyGameDealerTurn =>
                console.putStrLn(s"${s.game.forehand.name} has played : ${s.forehandCard}") >>
                  console.putStrLn("You can play one of the following card(s) from your hand :") >>
                  displayCardChoice[F](s.playableCards)
              case _: LateGameForehandTurn => console.putStrLn(s"Congrats, you have reached late game!")
            })
      })

  private def parseInput[F[_]](state: GameState, rawInput: String)(implicit F: Sync[F]): F[Input] = {
    rawInput match {
      case "\\q" => F.pure(End(state.game.forehand.id))
      case "\\r" => F.pure(Restart(state.game.forehand.id))
      case _ =>
        state match {
          case _: Init => F.pure(Start(state.game.forehand.id))
          case s: EarlyGameForehandTurn => parseEarlyGameForehandTurnChoice[F](s, rawInput)
          case s: EarlyGameDealerTurn => parseCardChoice[F](s, rawInput)
          case _: LateGameForehandTurn => F.pure(End(state.game.forehand.id))
          case _: LateGameForehandTurn => F.pure(End(state.game.forehand.id))
        }
    }
  }

  private def parseEarlyGameForehandTurnChoice[F[_]](state: EarlyGameForehandTurn, rawInput: String)(implicit F: Sync[F]): F[Input] =
    rawInput.toLowerCase match {
      case "c" => F.pure(CloseTalon(state.currentPlayer.id))
      case "j" if state.canExchangeTrumpJack => F.pure(ExchangeTrumpJack(state.currentPlayer.id))
      case _ => parseCardChoice[F](state, rawInput)
    }

  private def parseCardChoice[F[_] : Sync](state: PlayerTurn, rawInput: String): F[Input] = {
    val player = state.currentPlayer
    val playableCards = state.playableCards
    for {
      choice <- Sync[F].fromTry(Try { rawInput.toInt }).adaptError { case _ => InvalidInput }
      validChoice = choice > 0 && choice <= playableCards.length
      c <- if (validChoice) {
        val card = playableCards(choice - 1)
        Logger[F].debug(s"Player ${player.name} has played $card") >>
          PlayCard(player.id, card).pure[F]
      } else {
        Sync[F].raiseError[Input](InvalidInput)
      }
    } yield c
  }

}
