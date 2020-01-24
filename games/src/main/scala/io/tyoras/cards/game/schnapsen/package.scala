package io.tyoras.cards.game

import cats.data.StateT
import cats.syntax.functor._
import cats.{Applicative, ApplicativeError}
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards._
import io.tyoras.cards.game.schnapsen.model.Marriage.Status
import io.tyoras.cards.game.schnapsen.model.{DeckError, Game, Marriage, Player}

package object schnapsen {

  lazy val baseDeck: Deck = createDeck(allSuits, List(Ace(11), Ten(), King(4), Queen(3), Jack(2)))

  type PlayerId = FUUID

  private[schnapsen] type InternalGameState[F[_], A] = StateT[F, Game, A]

  private[schnapsen] def forehand[F[_] : Applicative]: InternalGameState[F, Player] = StateT.inspect { _.forehand }

  private[schnapsen] def dealer[F[_] : Applicative]: InternalGameState[F, Player] = StateT.inspect { _.dealer }

  private[schnapsen] def drawFirstCardF[F[_]](deck: Deck)(implicit F: ApplicativeError[F, Throwable]): F[(Card, Deck)] = {
    val (drawnCard, updatedDeck) = drawFirstCard(deck)
    for {
      c <- drawnCard.fold(F.raiseError[Card](DeckError("Not enough card left in the deck to draw.")))(F.pure)
    } yield (c, updatedDeck)
  }

  private[schnapsen] def findPossibleMarriages(hand: Hand, trumpSuit: Suit): List[Marriage] =
    hand
      .foldLeft[(List[Card], List[Marriage])]((Nil, Nil)) {
        case ((candidates, couples), card) if card.rank.isInstanceOf[King] || card.rank.isInstanceOf[Queen] =>
          candidates.partition(_.suit == card.suit) match {
            case (Nil, _) => (candidates :+ card, couples)
            case (_, remainingCandidates) => (remainingCandidates, couples :+ Marriage(card.suit, Status.of(trumpSuit, card.suit)))
          }
        case (res, _) => res
      }
      ._2

}
