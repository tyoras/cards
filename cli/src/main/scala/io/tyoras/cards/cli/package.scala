package io.tyoras.cards

import cats.Monad
import cats.effect.Console
import cats.implicits._

package object cli {

  val lineSeparator: String = "----------------------------------------------------------"

  def displayDeck[F[_] : Monad : Console](deck: Deck): F[Unit] = {
    val nbCards = deck.length / 4

    def displayCard(c: Card): F[Unit] = {
      val offset = if (c.rank.value == 10) "" else " "
      Console[F].putStr(s"$offset$c ")
    }

    def displaySplit(deck: Deck): F[Unit] = Monad[F].tailRecM(deck) {
      case Nil => ().asRight[Deck].pure[F]
      case d =>
        val (splitted, rest) = d.splitAt(nbCards)
        for {
          _ <- splitted.traverse_(displayCard)
          _ <- Console[F].putStrLn("")
        } yield rest.asLeft
    }

    displaySplit(deck)
  }

  def displayCardChoice[F[_] : Console](playableCards: Hand): F[Unit] = {
    val choices = playableCards.zipWithIndex.map {
      case (c: Card, i: Int) => s"\t${i + 1} : Play $c"
    }
    Console[F].putStrLn(s"${choices.mkString("\n")}")
  }
}
