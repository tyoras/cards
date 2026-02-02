package io.tyoras.cards.domain.game

import cats.data.StateT
import cats.effect.Sync
import cats.syntax.all.*
import cats.{Applicative, ApplicativeThrow}
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.game.schnapsen.model.Marriage.Status
import io.tyoras.cards.domain.game.schnapsen.model.*
import org.typelevel.log4cats.StructuredLogger

package object schnapsen:
  val schnapsenRanks: Set[Rank] = Set(Ace(11), Ten(), King(4), Queen(3), Jack(2))
  val schnapsenSuits: Set[Suit] = allSuits

  lazy val baseDeck: Deck = createDeck(schnapsenSuits, schnapsenRanks)

  type PlayerId = FUUID

  private[schnapsen] type InternalGameState[F[_], A] = StateT[F, GameRound, A]

  private[schnapsen] def forehand[F[_] : Applicative]: InternalGameState[F, Player] = StateT.inspect(_.forehand)

  private[schnapsen] def dealer[F[_] : Applicative]: InternalGameState[F, Player] = StateT.inspect(_.dealer)

  def drawFirstCardF[F[_] : ApplicativeThrow](deck: Deck): F[(Card, Deck)] =
    val (drawnCard, updatedDeck) = drawFirstCard(deck)
    for c <- drawnCard.fold(ApplicativeThrow[F].raiseError[Card](DeckError("Not enough card left in the deck to draw.")))(Applicative[F].pure)
    yield (c, updatedDeck)

  private[schnapsen] def findPossibleMarriages(hand: Hand, trumpSuit: Suit): List[Marriage] =
    hand
      .foldLeft[(List[Card], List[Marriage])]((Nil, Nil)) {
        case ((candidates, couples), card) if card.rank.isInstanceOf[King] || card.rank.isInstanceOf[Queen] =>
          candidates.partition(_.suit == card.suit) match
            case (Nil, _)                 => (candidates :+ card, couples)
            case (_, remainingCandidates) => (remainingCandidates, couples :+ Marriage(card.suit, Status.of(trumpSuit, card.suit)))
        case (res, _) => res
      }
      ._2

  private[schnapsen] def initGameRound[F[_]](ctx: GameContext)(logger: StructuredLogger[F])(using F: Sync[F]): F[GameRound] =
    def decideFirstDealer(context: GameContext): F[(PlayerId, PlayerId)] =
      val p1 = context.player1.id
      val p2 = context.player2.id

      def decide(d: Deck): F[(PlayerId, PlayerId)] = F.tailRecM(d) {
        case h1 :: h2 :: t if h1.rank.value == h2.rank.value => t.asLeft[(PlayerId, PlayerId)].pure[F]
        case h1 :: h2 :: _                                   => (if h1.rank.value > h2.rank.value then (p1, p2) else (p2, p1)).asRight[Deck].pure[F]
        case Nil                                             => decideFirstDealer(context).asRight[Deck].traverse(identity)
        case _                                               => F.raiseError(DeckError("Impossible to select first player : odd deck size"))
      }

      def swapDealer(previousDealerId: PlayerId): F[(PlayerId, PlayerId)] = previousDealerId match
        case context.player1.id => (p2, p1).pure[F]
        case context.player2.id => (p1, p2).pure[F]
        case _                  => F.raiseError(DeckError("Impossible to select first player : previous dealer is neither player 1 nor player 2"))

      for
        deck <- F.delay {
          shuffle(baseDeck)
        }
        decision <- context.previousFirstDealer.map(swapDealer).getOrElse(decide(deck))
      yield decision

    def dealing(deck: Deck): Either[DeckError, (Hand, Hand, Deck, Card)] =
      def deal(n: Int, talon: Deck): (Hand, Hand, Deck) =
        val (fhDraw, t)              = drawNCard(n, talon)
        val (dlDraw, remainingTalon) = drawNCard(n, t)
        (fhDraw, dlDraw, remainingTalon)

      val t0                               = shuffle(deck)
      val (fhFirstDraw, dlFirstDraw, t1)   = deal(3, t0)
      val (trumpCard, t2)                  = drawFirstCard(t1)
      val (fhSndDraw, dlSecondDraw, talon) = deal(2, t2)

      val dlHand = dlFirstDraw ++ dlSecondDraw
      val fhHand = fhFirstDraw ++ fhSndDraw
      trumpCard.toRight(DeckError("Not enough card left in the deck to draw.")).map((dlHand, fhHand, talon, _))

    for
      _        <- logger.debug("Starting new Schnapsen round")
      decision <- decideFirstDealer(ctx)
      (dealerId, forehandId) = decision
      updatedContext         = ctx.copy(previousFirstDealer = dealerId.some)
      dealt <- F.defer {
        F.fromEither(dealing(baseDeck))
      }
      (dlHand, fhHand, talon, trumpCard) = dealt
      dlInfo <- F.fromEither(ctx.player(dealerId))
      dealer = Player(dlInfo.id, dlInfo.name, dlHand)
      fhInfo <- F.fromEither(ctx.player(forehandId))
      forehand = Player(fhInfo.id, fhInfo.name, fhHand)
    yield GameRound(updatedContext, dealer, forehand, talon, trumpCard)
