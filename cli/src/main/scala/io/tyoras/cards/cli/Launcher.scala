package io.tyoras.cards.cli

import cats.effect.Console.implicits._
import cats.effect.Console.io._
import cats.effect.{ExitCode, IO, IOApp}
import io.tyoras.cards.cli.game.schnapsen.SchnapsenCli

object Launcher extends IOApp {

  val banner: String =
    """ _____               _
      |/  __ \             | |
      || /  \/ __ _ _ __ __| |    __ _  __ _ _ __ ___   ___  ___
      || |    / _` | '__/ _` |   / _` |/ _` | '_ ` _ \ / _ \/ __|
      || \__/\ (_| | | | (_| |  | (_| | (_| | | | | | |  __/\__ \
      | \____/\__,_|_|  \__,_|   \__, |\__,_|_| |_| |_|\___||___/
      |                           __/ |
      |                          |___/                           """.stripMargin

  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- putStrLn(banner)
    _ <- putStrLn(lineSeparator)
    //  WarCli.game()
    exitCode <- SchnapsenCli[IO].run
  } yield exitCode
}
