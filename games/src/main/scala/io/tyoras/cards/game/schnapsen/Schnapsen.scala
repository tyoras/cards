package io.tyoras.cards.game.schnapsen

import java.util.UUID

import cats._
import cats.data.StateT
import cats.effect.Sync
import cats.implicits._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.{Logger, SelfAwareStructuredLogger}
import io.tyoras.cards._

object Schnapsen {

  implicit def unsafeLogger[F[_] : Sync]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLogger[F]

  def initGame[F[_] : Sync](p1: Player, p2: Player): F[GameState] = for {
    _                  <- Logger[F].debug("Starting new Schnapsen game")
    (dealer, forehand) <- decideFirstDealer[F](p1, p2)
    initialState = dealing(dealer, forehand, baseDeck)
    _ <- Logger[F].debug(s"Initial state : $initialState")
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

  private def dealing(dealer: Player, forehand: Player, deck: Deck): Game = {
    def deal(n: Int, talon: Deck): (Hand, Hand, Deck) = {
      val (fhDraw, t) = takeNCard(n, talon)
      val (dlDraw, remainingTalon) = takeNCard(n, t)
      (fhDraw, dlDraw, remainingTalon)
    }

    val t0 = shuffle(deck)
    val (fhFirstDraw, dlFirstDraw, t1) = deal(3, t0)
    val (trumpCard, t2) = takeFirstCard(t1)
    val (fhSndDraw, dlSecondDraw, talon) = deal(2, t2)

    val d = dealer.copy(hand = dlFirstDraw ++ dlSecondDraw)
    val f = forehand.copy(hand = fhFirstDraw ++ fhSndDraw)
    Game(d, f, talon, trumpCard)
  }

  private def game[F[_] : Sync]: PartialFunction[(GameState, Input), F[GameState]] = {
    case (Init(g), _: Start) => Sync[F].pure(EarlyGameForehandTurn(g))
    case (s: EarlyGameForehandTurn, i: PlayCard) =>
      earlyGameForehandTurn(s, i).handleErrorWith {
        case se: SchnapsenError => Logger[F].warn(se)(s"Error during early game turn, ignoring input $i") *> Sync[F].pure(s)
      }
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

  private def earlyGameDealerTurn[F[_] : Sync](state: EarlyGameDealerTurn, input: PlayCard): F[GameState] = {

    def resolveTurn(fhCard: Card, dlCard: Card): InternalGameState[F, Unit] = for {
      _      <- playCard[F](state.currentPlayer, dlCard)
      winner <- findWinner[F](fhCard, dlCard)
      _      <- winTurn[F](winner, fhCard, dlCard)
      _      <- forehand[F] >>= drawCard[F]
      _      <- dealer[F] >>= drawCard[F]
    } yield ()

    def nextTurn(game: Game): F[GameState] = Sync[F].pure { EarlyGameForehandTurn(game) }

    def finishEarlyGame(game: Game): F[GameState] = {
      val (lastCard, updatedDeck) = takeFirstCard(game.talon)
      val updatedForehand = game.forehand.copy(hand = game.forehand.hand :+ lastCard)
      val updatedDealer = game.dealer.copy(hand = game.dealer.hand :+ game.trumpCard)
      val lateGame = game.copy(talon = updatedDeck, forehand = updatedForehand, dealer = updatedDealer)
      for {
        _ <- Logger[F].debug(s"Player ${updatedForehand.name} has drawn last card $lastCard")
        _ <- Logger[F].debug(s"Player ${updatedDealer.name} has drawn the trump card ${game.trumpCard}")
      } yield LateGameForehandTurn(lateGame) //TODO Select right player for forehand
    }

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

  private def playCard[F[_] : Sync](player: Player, playedCard: Card): InternalGameState[F, Card] = StateT { state =>
    val remainingHand = pickCard(playedCard, player.hand)
    val updatedPlayer = player.copy(hand = remainingHand)
    (state.updatePlayer(updatedPlayer), playedCard).pure[F]
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
    val (card, updatedDeck) = takeFirstCard(state.talon)
    val updatedPlayer = player.copy(hand = player.hand :+ card)
    val updatedState = state.updatePlayer(updatedPlayer).copy(talon = updatedDeck)
    for {
      _ <- Logger[F].debug(s"Player ${player.name} has drawn $card")
    } yield (updatedState, card)
  }

  private def checkPlayer[F[_]](expectedPlayer: Player, playerId: UUID)(implicit F: ApplicativeError[F, Throwable]): F[Unit] =
    if (expectedPlayer.id == playerId) F.unit else F.raiseError(WrongPlayer)

  private def checkPlayedCard[F[_]](card: Card, playableRule: Card => Boolean)(implicit F: ApplicativeError[F, Throwable]): F[Card] =
    if (playableRule.apply(card)) card.pure[F] else F.raiseError(InvalidCard)

}
