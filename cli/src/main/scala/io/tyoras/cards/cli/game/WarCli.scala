package io.tyoras.cards.cli.game

import cats.effect.{Console, ExitCode, Sync}
import cats.implicits._
import io.chrisdavenport.log4cats.SelfAwareStructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.tyoras.cards.cli.lineSeparator
import io.tyoras.cards.game.War.{divide, play}
import io.tyoras.cards.{international52Deck, shuffle}

trait WarCli[F[_]] {
  def run: F[ExitCode]
}

object WarCli {

  val banner =
    """ __          __
      | \ \        / /
      |  \ \  /\  / /_ _ _ __
      |   \ \/  \/ / _` | '__|
      |    \  /\  / (_| | |
      |     \/  \/ \__,_|_|   """.stripMargin

  def apply[F[_]](implicit F: Sync[F], console: Console[F]): WarCli[F] = new WarCli[F] {
    implicit val unsafeLogger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLogger[F]

    private val displayIntro: F[Unit] =
      console.putStrLn(banner) >>
        console.putStrLn(lineSeparator) >>
        console.putStrLn("")

    override val run: F[ExitCode] = {
      for {
        _ <- displayIntro
        _ <- F.delay(play(divide(shuffle(international52Deck))))
      } yield ExitCode.Success
    }
  }
}
