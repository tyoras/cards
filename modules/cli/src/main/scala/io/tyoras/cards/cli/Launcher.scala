package io.tyoras.cards.cli

import cats.effect.{ExitCode, IO}
import com.monovore.decline.*
import com.monovore.decline.effect.*
import io.tyoras.cards.BuildInfo
import io.tyoras.cards.cli.game.war.WarCli
import io.tyoras.cards.cli.game.schnapsen.SchnapsenCli
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import cats.syntax.all.*

object Launcher extends CommandIOApp(name = "cards", header = banner, version = s"cards version ${BuildInfo.version} built at ${BuildInfo.builtAtString}"):
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  case object SchnapsenCommand
  val schnapsenCommandOpts: Opts[SchnapsenCommand.type] =
    Opts.subcommand("schnapsen", "Play a game of Schnapsen") {
      Opts(SchnapsenCommand)
    }

  case class WarCommand(config: WarCli.Config)
  val playerCount: Opts[Int]         = Opts.option("player-count", "How many player for this game").withDefault(2)
  val autoNamePlayers: Opts[Boolean] = Opts.flag("auto-name", "Automatically use default name for players").orFalse
  val autoPlay: Opts[Boolean]        = Opts.flag("auto-play", "Automatically play").orFalse
  val warCommandOpts: Opts[WarCommand] =
    Opts.subcommand("war", "Play a game of War") {
      (playerCount, autoNamePlayers, autoPlay).mapN(WarCli.Config.apply).map(WarCommand.apply)
    }

  override def main: Opts[IO[ExitCode]] =
    schnapsenCommandOpts
      .orElse(warCommandOpts)
      .map {
        case SchnapsenCommand => SchnapsenCli[IO].run
        case WarCommand(cfg)  => WarCli[IO](cfg).run
      }
      .map(_.onError(e => IO.println(s"Unexpected error: $e")))
      .map {
        displayBanner[IO] >> _
      }
