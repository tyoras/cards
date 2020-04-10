package io.tyoras.cards.cli

import cats.effect.Console.implicits._
import cats.effect.{ExitCode, IO}
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import io.tyoras.cards.BuildInfo
import io.tyoras.cards.cli.game.WarCli
import io.tyoras.cards.cli.game.schnapsen.SchnapsenCli

object Launcher
  extends CommandIOApp(name = "card-games", header = banner, version = s"card-games version ${BuildInfo.version} built at ${BuildInfo.builtAtString}") {

  case object SchnapsenCommand
  val schnapsenCommandOpts: Opts[SchnapsenCommand.type] =
    Opts.subcommand("schnapsen", "Play a game of Schnapsen") {
      Opts(SchnapsenCommand)
    }

  case object WarCommand
  val warCommandOpts: Opts[WarCommand.type] =
    Opts.subcommand("war", "Play a game of War") {
      Opts(WarCommand)
    }

  override def main: Opts[IO[ExitCode]] = {
    (schnapsenCommandOpts orElse warCommandOpts).map {
      case SchnapsenCommand => SchnapsenCli[IO].run
      case WarCommand => WarCli[IO].run
    }.map {
      displayBanner[IO] >> _
    }
  }

}
