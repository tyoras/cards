package io.tyoras.cards.cli.game.war

import cats.effect.std.Console
import cats.effect.{Async, ExitCode, Sync}
import cats.syntax.all.*
import io.tyoras.cards.cli.game.war.WarCliError.*
import io.tyoras.cards.cli.lineSeparator
import io.tyoras.cards.domain.game.schnapsen.PlayerId
import io.tyoras.cards.domain.game.war.*
import io.tyoras.cards.domain.game.war.model.GameState
import io.tyoras.cards.domain.game.war.model.GameState.*
import org.typelevel.log4cats.*

trait WarCli[F[_]]:
  def run: F[ExitCode]

object WarCli:
  final case class Config(defaultPlayerCount: PlayerCount, autoNamePlayers: Boolean, autoPlay: Boolean)

  val banner: String =
    """ __          __
      | \ \        / /
      |  \ \  /\  / /_ _ _ __
      |   \ \/  \/ / _` | '__|
      |    \  /\  / (_| | |
      |     \/  \/ \__,_|_|   """.stripMargin

  def apply[F[_] : Async : LoggerFactory](config: Config)(using console: Console[F]): WarCli[F] =
    val logger      = LoggerFactory.getLogger
    val inputParser = InputParser[F]
    val rendering   = Rendering[F]

    new WarCli[F]:
      private val displayIntro: F[Unit] =
        console.println(banner) >>
          console.println(lineSeparator) >>
          console.println("Multi players game") >>
          console.println("At any moment you can use \\q to quit the game or \\r to restart it.") >>
          console.println(lineSeparator)

      private val readPlayerCount: F[PlayerCount] =
        console.readLine.flatMap {
          case "" => config.defaultPlayerCount.pure
          case str =>
            Sync[F]
              .fromEither(PlayerCount.from(str))
              .handleErrorWith { err =>
                logger.warn(err)(s"Incorrect player count input $str") *>
                  console.println(s"$str is not a valid number! Please try again:") >> readPlayerCount
              }
              .flatTap(c => console.println(s"Incorrect player count, it must be a number between 2 and 52. You entered $c"))
        }

      private val askPlayerInfos: F[List[String]] =
        for
          _           <- console.println(s"How many players for this game? (default : ${config.defaultPlayerCount})")
          playerCount <- readPlayerCount <* console.println(lineSeparator)
          names       <- (1 to playerCount.value).toList.traverse(i => askPlayerName(s"Player $i"))
        yield names

      private def askPlayerName(defaultName: String): F[String] =
        if config.autoNamePlayers then defaultName.pure
        else
          console.println(s"What is the name of this player? (default : $defaultName)") >>
            console.readLine.map(_.trim).flatMap {
              case name if name.nonEmpty => name.pure
              case name                  => console.println(s"Using default name $defaultName for this player").as(defaultName)
            } <* console.println(lineSeparator)

      private def collectRawInput(state: GameState): F[String] = state match
        case _: Finish            => console.readLine
        case _ if config.autoPlay => "".pure
        case _                    => console.readLine

      override val run: F[ExitCode] =
        def loop(game: War[F]): F[War[F]] =
          (for
            state      <- game.currentState
            playerId   <- Sync[F].fromEither(currentPlayer(state).left.map(UnexpectedError(_)))
            _          <- rendering.renderGameState(state, playerId)
            rawInput   <- collectRawInput(state)
            gameInputs <- inputParser.parse(state, playerId, rawInput)
            _          <- gameInputs.traverse_(game.submitInput)
          yield ())
            .recoverWith { case InvalidInput =>
              console.println("Your last input is invalid, try again.")
            }
            .as(game)

        for
          _           <- displayIntro
          playerNames <- askPlayerInfos
          game        <- War(playerNames)
          exitCode <- game.tailRecM(_.currentState.flatMap {
            case Exit(_) => ExitCode.Success.asRight.pure
            case _       => loop(game).map(_.asLeft)
          })
        yield exitCode

      private def currentPlayer(state: GameState): Either[String, PlayerId] =
        state match
          case s: Init          => s.notReady.headOption.toRight("Init state with everyone ready")
          case s: BattleTurn    => s.missingPlays.headOption.toRight("Player turn state when they all have already played")
          case s: WarTurn       => s.missingPlays.headOption.toRight("Player turn state when they all have already played")
          case s: PlayerWinTurn => s.notAcked.headOption.toRight("Player turn win state with everyone acked")
          case s: Finish        => s.winnerId.asRight
          case s                => s"Unable to find current player for state: $s".asLeft
