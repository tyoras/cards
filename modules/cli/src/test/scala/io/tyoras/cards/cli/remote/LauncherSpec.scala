package io.tyoras.cards.cli.remote

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{ExitCode, IO}
import com.monovore.decline.{Command, Opts}
import io.tyoras.cards.cli.remote.Launcher.{ERsCommand, SchnapsenCommand, WarCommand}
import io.tyoras.cards.cli.remote.game.war.WarCli
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** Only the commands that do not start an interactive terminal UI are run here */
class LauncherSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private def parse[A](opts: Opts[A], args: String*) = Command("cards", "test")(opts).parse(args)

  "Launcher" should "parse the war command" in {
    IO((parse(Launcher.warCommandOpts, "war"), parse(Launcher.warCommandOpts, "war", "--auto-play"))).asserting { (manual, auto) =>
      manual shouldBe Right(WarCommand(WarCli.Config(autoPlay = false)))
      auto shouldBe Right(WarCommand(WarCli.Config(autoPlay = true)))
    }
  }

  it should "parse the schnapsen command" in
    IO(parse(Launcher.schnapsenCommandOpts, "schnapsen")).asserting(_ shouldBe Right(SchnapsenCommand))

  it should "parse the egyptian ratscrew command" in
    IO(parse(Launcher.ersCommandOpts, "egyptian_ratscrew")).asserting(_ shouldBe Right(ERsCommand))

  it should "accept the war command, the schnapsen command, the egyptian ratscrew command and no command at all" in {
    val commands = List(Nil, List("war"), List("war", "--auto-play"), List("schnapsen"), List("egyptian_ratscrew"))
    IO(commands.map(args => parse(Launcher.main, args*))).asserting { results =>
      every(results.map(_.isRight)) shouldBe true
    }
  }

  it should "reject unknown commands" in
    IO(parse(Launcher.main, "poker")).asserting(_.isLeft shouldBe true)

  it should "report that schnapsen is not implemented yet" in
    parse(Launcher.main, "schnapsen").fold(h => IO.raiseError(new Exception(h.toString)), identity).asserting(_ shouldBe ExitCode.Success)
