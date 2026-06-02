package io.tyoras.cards.cli.remote

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import com.monovore.decline.*
import com.monovore.decline.effect.*
import io.tyoras.cards.BuildInfo
import io.tyoras.cards.cli.*
import io.tyoras.cards.cli.remote.game.war.WarCli
import io.tyoras.cards.cli.remote.tui.LauncherTUI
import io.tyoras.cards.cli.tui.TUI
import io.tyoras.cards.domain.game.GameTyp
import io.tyoras.cards.domain.game.GameTyp.{Schnapsen, War}
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

object Launcher extends CommandIOApp(name = "cards", header = banner, version = s"cards version ${BuildInfo.version} built at ${BuildInfo.builtAtString}"):
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  case object SchnapsenCommand

  val schnapsenCommandOpts: Opts[SchnapsenCommand.type] =
    Opts.subcommand("schnapsen", "Play a game of Schnapsen") {
      Opts(SchnapsenCommand)
    }

  case class WarCommand(config: WarCli.Config)
  val autoPlay: Opts[Boolean]          = Opts.flag("auto-play", "Automatically play").orFalse
  val warCommandOpts: Opts[WarCommand] =
    Opts.subcommand("war", "Play a game of War") {
      autoPlay.map(WarCli.Config.apply).map(WarCommand.apply)
    }

  override def main: Opts[IO[ExitCode]] =
    schnapsenCommandOpts
      .orElse(warCommandOpts)
      .map {
        case SchnapsenCommand => IO.println("Not implemented yet").as(ExitCode.Success)
        case WarCommand(cfg)  => WarCli.make[IO](cfg).flatMap(_.run)
      }
      .withDefault(gameSelection)
      .map(_.onError(e => Console[IO].errorln(s"Unexpected error: $e")))

  private def gameSelection: IO[ExitCode] =
    for
      gameChoice <- IO.deferred[GameTyp[?, ?]]
      _          <- TUI.runTUI(LauncherTUI.make(gameChoice))
      selected   <- gameChoice.get
      exitCode   <- launchGame(selected)
    yield exitCode

  private def launchGame(gameChoice: GameTyp[?, ?]): IO[ExitCode] =
    gameChoice match
      case War       => WarCli.make[IO](WarCli.Config(false)).flatMap(_.run)
      case Schnapsen => IO.println("Not implemented yet").as(ExitCode.Success)
