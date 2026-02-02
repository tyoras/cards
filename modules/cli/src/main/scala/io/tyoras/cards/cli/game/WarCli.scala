package io.tyoras.cards.cli.game

import cats.effect.std.Console
import cats.effect.{ExitCode, Sync}
import cats.syntax.all.*
import io.tyoras.cards.cli.lineSeparator
import io.tyoras.cards.domain.card.{international52Deck, shuffle}
import io.tyoras.cards.domain.game.war.War.{divide, play}

trait WarCli[F[_]]:
  def run: F[ExitCode]

object WarCli:

  val banner =
    """ __          __
      | \ \        / /
      |  \ \  /\  / /_ _ _ __
      |   \ \/  \/ / _` | '__|
      |    \  /\  / (_| | |
      |     \/  \/ \__,_|_|   """.stripMargin

  def apply[F[_] : Sync](using console: Console[F]): WarCli[F] = new WarCli[F] {

    private val displayIntro: F[Unit] =
      console.println(banner) >>
        console.println(lineSeparator) >>
        console.println("")

    override val run: F[ExitCode] =
      for
        _ <- displayIntro
        _ <- Sync[F].delay(play(divide(shuffle(international52Deck))))
      yield ExitCode.Success
  }
