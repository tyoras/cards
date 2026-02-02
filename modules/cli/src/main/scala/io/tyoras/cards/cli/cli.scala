package io.tyoras.cards.cli

import cats.Monad
import cats.effect.std.Console
import cats.syntax.all.*
import io.tyoras.cards.domain.card.{Card, Deck, Hand}

val banner: String =
  """ _____               _
    |/  __ \             | |
    || /  \/ __ _ _ __ __| |    __ _  __ _ _ __ ___   ___  ___
    || |    / _` | '__/ _` |   / _` |/ _` | '_ ` _ \ / _ \/ __|
    || \__/\ (_| | | | (_| |  | (_| | (_| | | | | | |  __/\__ \
    | \____/\__,_|_|  \__,_|   \__, |\__,_|_| |_| |_|\___||___/
    |                           __/ |
    |                          |___/                           """.stripMargin

val lineSeparator: String = "-" * 58

def displayBanner[F[_] : Monad : Console]: F[Unit] = for
  _ <- Console[F].println(banner)
  _ <- Console[F].println(lineSeparator)
yield ()

def displayDeck[F[_] : Monad : Console](deck: Deck): F[Unit] =
  val nbCards = deck.length / 4

  def displayCard(c: Card): F[Unit] =
    val offset = if c.rank.value == 10 then "" else " "
    Console[F].print(s"$offset$c ")

  def displaySplit(deck: Deck): F[Unit] = Monad[F].tailRecM(deck) {
    case Nil => ().asRight[Deck].pure[F]
    case d =>
      val (split, rest) = d.splitAt(nbCards)
      for
        _ <- split.traverse_(displayCard)
        _ <- Console[F].println("")
      yield rest.asLeft
  }

  displaySplit(deck)

def displayCardChoice[F[_] : Console](playableCards: Hand): F[Unit] =
  val choices = playableCards.zipWithIndex.map { case (c: Card, i: Int) =>
    s"\t${i + 1} : Play $c"
  }
  Console[F].println(s"${choices.mkString("\n")}")
