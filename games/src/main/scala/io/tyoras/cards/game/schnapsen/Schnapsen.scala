package io.tyoras.cards.game.schnapsen

import cats._
import cats.data.StateT
import cats.effect.Sync
import cats.implicits._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.{Logger, SelfAwareStructuredLogger}
import io.tyoras.cards.{drawFirstCard, _}

object Schnapsen {

  implicit def unsafeLogger[F[_] : Sync]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLogger[F]

  def initGame[F[_] : Sync](p1: Player, p2: Player): F[GameState] = for {
    _                  <- Logger[F].debug("Starting new Schnapsen game")
    (dealer, forehand) <- decideFirstDealer[F](p1, p2)
    initialState       <- Sync[F].fromEither(dealing(dealer, forehand, baseDeck))
    _                  <- Logger[F].debug(s"Initial state : $initialState")
  } yield Init(initialState)

  def submitInput[F[_] : Sync](state: GameState, input: Input): F[GameState] =
    Logger[F].debug(s"Submitting input $input on current state : $state") >>
      (menu[F] orElse game applyOrElse (
        (state, input),
        (_: (GameState, Input)) => Logger[F].debug(s"Ignoring wrong input : $input") *> state.pure[F]
      ))

  private def menu[F[_] : Sync]: PartialFunction[(GameState, Input), F[GameState]] = {
    case (s, _: Restart) =>
      Logger[F].debug("Restarting new game") >>
        initGame(s.game.forehand, s.game.dealer)
    case (s, _: End) =>
      Logger[F].debug("Ending the game") >>
        Sync[F].pure(Exit(s.game))
  }

  private def decideFirstDealer[F[_]](p1: Player, p2: Player)(implicit F: Sync[F]): F[(Player, Player)] = {
    def decide(d: Deck): F[(Player, Player)] = F.tailRecM(d) {
      case h1 :: h2 :: t if h1.rank.value == h2.rank.value => t.asLeft[(Player, Player)].pure[F]
      case h1 :: h2 :: _ => (if (h1.rank.value > h2.rank.value) (p1, p2) else (p2, p1)).asRight[Deck].pure[F]
      case Nil => decideFirstDealer(p1, p2).asRight[Deck].traverse(identity)
      case _ => F.raiseError(DeckError("Impossible to select first player : odd deck size"))
    }

    for {
      deck     <- F.delay { shuffle(baseDeck) }
      decision <- decide(deck)
    } yield decision
  }

  private def dealing(dealer: Player, forehand: Player, deck: Deck): Either[DeckError, Game] = {
    def deal(n: Int, talon: Deck): (Hand, Hand, Deck) = {
      val (fhDraw, t) = drawNCard(n, talon)
      val (dlDraw, remainingTalon) = drawNCard(n, t)
      (fhDraw, dlDraw, remainingTalon)
    }

    val t0 = shuffle(deck)
    val (fhFirstDraw, dlFirstDraw, t1) = deal(3, t0)
    val (trumpCard, t2) = drawFirstCard(t1)
    val (fhSndDraw, dlSecondDraw, talon) = deal(2, t2)

    val d = dealer.copy(hand = dlFirstDraw ++ dlSecondDraw)
    val f = forehand.copy(hand = fhFirstDraw ++ fhSndDraw)
    trumpCard.toRight(DeckError("Not enough card left in the deck to draw.")).map(Game(d, f, talon, _))
  }

  private def game[F[_] : Sync]: PartialFunction[(GameState, Input), F[GameState]] = {
    case (Init(g), _: Start) => Sync[F].pure(EarlyGameForehandTurn(g))
    case (s: EarlyGameForehandTurn, i: PlayCard) =>
      earlyGameForehandTurn(s, i).handleErrorWith {
        case se: SchnapsenError => Logger[F].warn(se)(s"Error during early game turn, ignoring input $i") *> Sync[F].pure(s)
      }
    case (s: EarlyGameForehandTurn, i: ExchangeTrumpJack) => exchangeTrumpJack(s, i)
    case (s: EarlyGameForehandTurn, i: CloseTalon) => closeTalon(s, i)
    case (s: EarlyGameDealerTurn, i: PlayCard) =>
      earlyGameDealerTurn(s, i).handleErrorWith {
        case se: SchnapsenError => Logger[F].warn(se)(s"Error during early game turn, ignoring input $i") *> Sync[F].pure(s)
      }
  }

  private def earlyGameForehandTurn[F[_] : Sync](state: EarlyGameForehandTurn, input: PlayCard): F[GameState] = {
    val forehand = state.currentPlayer
    for {
      _           <- checkPlayer[F](forehand, input.playerId)
      card        <- checkPlayedCard[F](input.card, state.playableCards.contains(_))
      updatedGame <- playCard(forehand, card).runS(state.game)
    } yield EarlyGameDealerTurn(updatedGame, card)
  }

  private def exchangeTrumpJack[F[_]](state: EarlyGameForehandTurn, input: ExchangeTrumpJack)(implicit F: Sync[F]): F[GameState] = {
    val forehand = state.currentPlayer
    val trumpCard = state.game.trumpCard
    for {
      _ <- checkPlayer[F](forehand, input.playerId)
      _ <- if (state.canExchangeTrumpJack) F.unit else F.raiseError[Unit](InvalidAction())
      remainingHand = pickCard(state.trumpJack, forehand.hand)._2
      updatedForehand = forehand.copy(hand = remainingHand :+ trumpCard)
      updatedGame = state.game.copy(trumpCard = state.trumpJack, forehand = updatedForehand)
      _ <- Logger[F].debug(s"Player ${forehand.name} has exchanged the trump jack ${state.trumpJack} with the trump card $trumpCard")
    } yield EarlyGameForehandTurn(updatedGame)
  }

  private def closeTalon[F[_]](state: EarlyGameForehandTurn, input: CloseTalon)(implicit F: Sync[F]): F[GameState] = {
    val forehand = state.currentPlayer
    for {
      _ <- checkPlayer[F](forehand, input.playerId)
      lateGame = state.game.copy(talonClosedBy = forehand.id.some)
      _ <- Logger[F].debug(s"Player ${forehand.name} has closed the talon.")
    } yield LateGameForehandTurn(lateGame)
  }

  private def earlyGameDealerTurn[F[_] : Sync](state: EarlyGameDealerTurn, input: PlayCard): F[GameState] = {

    def resolveTurn(fhCard: Card, dlCard: Card): InternalGameState[F, Unit] = for {
      _      <- playCard[F](state.currentPlayer, dlCard)
      winner <- findWinner[F](fhCard, dlCard)
      _      <- winTurn[F](winner, fhCard, dlCard)
      _      <- forehand[F] >>= drawCard[F]
      _      <- dealer[F] >>= drawCard[F]
    } yield ()

    def nextTurn(game: Game): F[GameState] = Sync[F].pure { EarlyGameForehandTurn(game) }

    def finishEarlyGame(game: Game): F[GameState] = for {
      (lastCard, updatedDeck) <- drawFirstCardF[F](game.talon)
      updatedForehand = game.forehand.copy(hand = game.forehand.hand :+ lastCard)
      updatedDealer = game.dealer.copy(hand = game.dealer.hand :+ game.trumpCard)
      lateGame = game.copy(talon = updatedDeck, forehand = updatedForehand, dealer = updatedDealer)
      _ <- Logger[F].debug(s"Player ${updatedForehand.name} has drawn last card $lastCard")
      _ <- Logger[F].debug(s"Player ${updatedDealer.name} has drawn the trump card ${game.trumpCard}")
    } yield LateGameForehandTurn(lateGame) //TODO Select right player for forehand

    for {
      _           <- checkPlayer[F](state.currentPlayer, input.playerId)
      dealerCard  <- checkPlayedCard[F](input.card, state.playableCards.contains(_))
      updatedGame <- resolveTurn(state.forehandCard, dealerCard).runS(state.game)
      nextState <- updatedGame.talon match {
        case _ :: Nil => finishEarlyGame(updatedGame)
        case _ => nextTurn(updatedGame)
      }
    } yield nextState
  }

  private def playCard[F[_] : Sync](player: Player, card: Card): InternalGameState[F, Card] = StateT { state =>
    val (playedCard, remainingHand) = pickCard(card, player.hand)
    playedCard match {
      case None => Sync[F].raiseError(InvalidCard(s"Player ${player.name} has tried to play the card $card that he does not own."))
      case Some(c) =>
        val updatedPlayer = player.copy(hand = remainingHand)
        (state.updatePlayer(updatedPlayer), c).pure[F]
    }
  }

  private def findWinner[F[_] : Applicative](fhCard: Card, dlCard: Card): InternalGameState[F, Player] = StateT { state =>
    val trumpSuit = state.trumpCard.suit
    val winner = if ((dlCard.suit == fhCard.suit && dlCard.rank > fhCard.rank) || (dlCard.suit == trumpSuit)) state.dealer else state.forehand
    (state, winner).pure[F]
  }

  private def winTurn[F[_] : Applicative](winner: Player, c1: Card, c2: Card): InternalGameState[F, Unit] = StateT { state =>
    val updatedScore = winner.score + c1.rank.value + c2.rank.value
    val updatedWonCards = winner.wonCards ++ List(c1, c2)
    val updatedWinner = winner.copy(wonCards = updatedWonCards, score = updatedScore)
    val updatedState = if (winner == state.forehand) {
      state.copy(forehand = updatedWinner)
    } else {
      state.copy(dealer = state.forehand, forehand = updatedWinner)
    }
    (updatedState, ()).pure[F]
  }

  private def drawCard[F[_] : Sync](player: Player): InternalGameState[F, Card] = StateT { state =>
    for {
      (card, updatedDeck) <- drawFirstCardF[F](state.talon)
      _                   <- Logger[F].debug(s"Player ${player.name} has drawn $card")
      updatedPlayer = player.copy(hand = player.hand :+ card)
      updatedState = state.updatePlayer(updatedPlayer).copy(talon = updatedDeck)
    } yield (updatedState, card)
  }

  private def checkPlayer[F[_]](expectedPlayer: Player, playerId: PlayerId)(implicit F: ApplicativeError[F, Throwable]): F[Unit] =
    if (expectedPlayer.id == playerId) F.unit else F.raiseError(WrongPlayer)

  private def checkPlayedCard[F[_]](card: Card, playableRule: Card => Boolean)(implicit F: ApplicativeError[F, Throwable]): F[Card] =
    if (playableRule.apply(card)) card.pure[F] else F.raiseError(InvalidCard())

}
