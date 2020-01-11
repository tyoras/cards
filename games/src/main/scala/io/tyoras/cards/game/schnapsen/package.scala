package io.tyoras.cards.game

import cats.data.StateT
import cats.syntax.functor._
import cats.{Applicative, ApplicativeError}
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards._

package object schnapsen {

  lazy val baseDeck: Deck = createDeck(allSuits, List(Ace(11), Ten(), King(4), Queen(3), Jack(2)))

  sealed trait Role

  case object Forehand extends Role

  case object Dealer extends Role

  type PlayerId = FUUID

  case class Player(id: PlayerId, name: String, hand: Hand = Nil, score: Int = 0, wonCards: Hand = Nil) {
    override def toString = s"name = $name ($id) \t| score = $score \t| hand = ${hand.mkString(" ")}"
  }

  case class Game(dealer: Player, forehand: Player, talon: Deck, trumpCard: Card, talonClosedBy: Option[PlayerId] = None) {
    override def toString: String =
      s"""
         |\tDealer\t\t$dealer
         |\tForehand\t$forehand
         |\tTrump card\t$trumpCard
         |\tTalon\t\t${if (talon.isEmpty) "empty" else talon.mkString(" ")}\t| status = ${talonClosedBy.fold("open")(p => s"closed by ${playersById(p).name}")}""".stripMargin

    val roles: Map[Role, Player] = Map(Forehand -> forehand, Dealer -> dealer)
    val playersById: Map[PlayerId, Player] = Map(forehand.id -> forehand, dealer.id -> dealer)

    def updatePlayer(updatedPlayer: Player): Game =
      if (updatedPlayer.id == dealer.id)
        copy(dealer = updatedPlayer)
      else
        copy(forehand = updatedPlayer)
  }

  private[schnapsen] type InternalGameState[F[_], A] = StateT[F, Game, A]

  private[schnapsen] def forehand[F[_] : Applicative]: InternalGameState[F, Player] = StateT.inspect { _.forehand }

  private[schnapsen] def dealer[F[_] : Applicative]: InternalGameState[F, Player] = StateT.inspect { _.dealer }

  sealed abstract class SchnapsenError(code: String, msg: String) extends GameError(code, msg)
  case class DeckError(msg: String) extends SchnapsenError("deck_error", msg)
  case object WrongPlayer extends SchnapsenError("wrong_player", "Another player than the expected one has tried to play.")
  case class InvalidAction(msg: String = "The player has tried to play an invalid action.") extends SchnapsenError("invalid_action", msg)
  case class InvalidCard(msg: String = "The player has tried to play an invalid card.") extends SchnapsenError("invalid_card", msg)

  private[schnapsen] def drawFirstCardF[F[_]](deck: Deck)(implicit F: ApplicativeError[F, Throwable]): F[(Card, Deck)] = {
    val (drawnCard, updatedDeck) = drawFirstCard(deck)
    for {
      c <- drawnCard.fold(F.raiseError[Card](DeckError("Not enough card left in the deck to draw.")))(F.pure)
    } yield (c, updatedDeck)
  }

}
