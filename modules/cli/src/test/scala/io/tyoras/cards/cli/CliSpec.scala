package io.tyoras.cards.cli

import cats.effect.IO
import cats.effect.std.Console
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.testkit.TestConsole
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.{Card, international52Deck}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class CliSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  /** Run the program with a test console, returning what it wrote to the standard output */
  private def stdOutOf(program: Console[IO] ?=> IO[Unit]): IO[String] =
    TestConsole.resource[IO].use { (console, _, inspector) =>
      program(using console) >> inspector.stdOut
    }

  "banner" should "be followed by the line separator when displayed" in
    stdOutOf(displayBanner[IO]).asserting(_ shouldBe s"$banner\n$lineSeparator\n")

  "lineSeparator" should "be 58 dashes long" in
    IO(lineSeparator).asserting(_ shouldBe "-" * 58)

  "displayDeck" should "display the deck in 4 lines" in
    stdOutOf(displayDeck[IO](international52Deck)).asserting { out =>
      val lines = out.split("\n").toList
      lines should have size 4
      every(lines.map(_.trim.split(" +").length)) shouldBe 13
    }

  it should "not add an offset before ten cards" in {
    val deck = List(Card(Heart, Ten()), Card(Spade, Two()), Card(Club, Ace()), Card(Diamond, King()))
    stdOutOf(displayDeck[IO](deck)).asserting {
      _ shouldBe s"${Card(Heart, Ten())} \n ${Card(Spade, Two())} \n ${Card(Club, Ace())} \n ${Card(Diamond, King())} \n"
    }
  }

  it should "display nothing for an empty deck" in
    stdOutOf(displayDeck[IO](Nil)).asserting(_ shouldBe "")

  "displayCardChoice" should "list the playable cards with their index" in {
    val cards = List(Card(Heart, Ace()), Card(Spade, Two()))
    stdOutOf(displayCardChoice[IO](cards)).asserting(_ shouldBe s"\t1 : Play ${cards.head.emoji}\n\t2 : Play ${cards(1).emoji}\n")
  }
