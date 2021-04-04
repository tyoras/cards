package io.tyoras.cards.game.schnapsen

import cats.data.StateT
import cats.effect.{Clock, Concurrent, Sync}
import cats.implicits._
import io.chrisdavenport.cats.effect.time.implicits.ClockOps
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, StructuredLogger}
import io.tyoras.cards.game.schnapsen.model.Marriage.Status
import io.tyoras.cards.game.schnapsen.model._
import io.tyoras.cards.util.fsm.FinalStateMachine
import io.tyoras.cards.util.fsm.concurrent.SynchronizedConcurrentFSM
import io.tyoras.cards._

trait Schnapsen[F[_]] {
  def currentState: F[GameState]
  def submitInput(input: Input): F[GameState]
}

object Schnapsen {

  def apply[F[_] : Clock : Concurrent](context: GameContext): F[Schnapsen[F]] = for {
    implicit0(logger: StructuredLogger[F]) <- Slf4jLogger.create[F]
    _                                      <- logger.debug("Starting new Schnapsen game")
    initialRound                           <- initGameRound(context)
    _                                      <- logger.debug(s"Initial round : $initialRound")
    initialState = Init(initialRound)
    fsm <- SynchronizedConcurrentFSM.create[F, GameState](initialState)
  } yield new SchnapsenImplem[F](fsm)
}

private class SchnapsenImplem[F[_] : Sync : Clock : StructuredLogger](fsm: FinalStateMachine[F, GameState]) extends Schnapsen[F] {

  override def submitInput(input: Input): F[GameState] = fsm.transition { s =>
    Logger[F].debug(s"Submitting input $input on current state : $s") >>
      menu
        .orElse(game)
        .applyOrElse(
          s -> input,
          (_: (GameState, Input)) => Logger[F].debug(s"Ignoring wrong input : $input") *> s.pure[F]
        )
  }

  private def menu: PartialFunction[(GameState, Input), F[GameState]] = {
    case (s, restart: Restart) =>
      Logger[F].debug(s"Player ${restart.playerId} has asked to restart a new game") >>
        Clock[F].getZonedDateTimeUTC.flatMap(now => initGameRound(GameContext.reset(s.round.context, now))).map(newRound => EarlyGameForehandTurn(newRound))
    case (s, end: End) =>
      Logger[F].debug(s"Player ${end.playerId} has asked to exit the game").as(Exit(s.round))
  }

  private def game: PartialFunction[(GameState, Input), F[GameState]] = {
    case (Init(g), _: Start) => Sync[F].pure(EarlyGameForehandTurn(g))
    case (s: ForehandTurn, i: PlayCard) =>
      forehandTurn(s, i).handleErrorWith { case se: SchnapsenError =>
        Logger[F].warn(se)(s"Error during turn, ignoring input $i") *> Sync[F].pure(s)
      }
    case (s: ForehandTurn, i: ClaimVictory) => claimVictory(s, i)
    case (s: EarlyGameForehandTurn, i: ExchangeTrumpJack) => exchangeTrumpJack(s, i)
    case (s: EarlyGameForehandTurn, i: CloseTalon) => closeTalon(s, i)
    case (s: EarlyGameForehandTurn, i: Meld) => marriage(s, i)
    case (s: DealerTurn, i: PlayCard) =>
      dealerTurn(s, i).handleErrorWith { case se: SchnapsenError =>
        Logger[F].warn(se)(s"Error during dealer turn, ignoring input $i") *> Sync[F].pure(s)
      }
    case (s: LateGameForehandTurn, i: Meld) => marriage(s, i)
    case (Finish(r, _), _: Start) => initGameRound[F](r.context).map(newRound => EarlyGameForehandTurn(newRound))
  }

  private def forehandTurn(state: ForehandTurn, input: PlayCard): F[GameState] = {
    val forehand = state.currentPlayer
    for {
      _           <- checkPlayer(forehand, input.playerId)
      card        <- checkPlayedCard(input.card, state.playableCards.contains(_))
      updatedGame <- playCard(forehand, card).runS(state.round)
    } yield state match {
      case _: EarlyGame => EarlyGameDealerTurn(updatedGame, card)
      case _: LateGame => LateGameDealerTurn(updatedGame, card)
    }
  }

  private def exchangeTrumpJack(state: EarlyGameForehandTurn, input: ExchangeTrumpJack): F[GameState] = {
    val forehand = state.currentPlayer
    val trumpCard = state.round.trumpCard
    for {
      _ <- checkPlayer(forehand, input.playerId)
      _ <- if (state.canExchangeTrumpJack) F.unit else F.raiseError[Unit](InvalidAction())
      remainingHand = pickCard(state.trumpJack, forehand.hand)._2
      updatedForehand = forehand.copy(hand = remainingHand :+ trumpCard)
      updatedRound = state.round.copy(trumpCard = state.trumpJack, forehand = updatedForehand)
      _ <- Logger[F].debug(s"Player ${forehand.name} has exchanged the trump jack ${state.trumpJack} with the trump card $trumpCard")
    } yield EarlyGameForehandTurn(updatedRound)
  }

  private def closeTalon(state: EarlyGameForehandTurn, input: CloseTalon): F[GameState] = {
    val forehand = state.currentPlayer
    val dealer = state.round.dealer
    for {
      _ <- checkPlayer(forehand, input.playerId)
      _ <- F.whenA(state.ongoingMarriage.isDefined)(F.raiseError[Unit](InvalidAction()))
      lateGame = state.round.copy(talonClosing = TalonClosing(forehand.id, dealer.score).some)
      _ <- Logger[F].debug(s"Player ${forehand.name} has closed the talon.")
    } yield LateGameForehandTurn(lateGame)
  }

  private def marriage(state: ForehandTurn, input: Meld): F[GameState] = {
    val forehand = state.currentPlayer
    for {
      _ <- checkPlayer(forehand, input.playerId)
      marriage = Marriage(input.suit, Status.of(state.round.trumpSuit, input.suit))
      wonPoints = if (forehand.wonCards.isEmpty) 0 else marriage.status.score
      updatedForehand = forehand.copy(score = forehand.score + wonPoints, marriages = forehand.marriages :+ marriage)
      updatedRound = state.round.copy(forehand = updatedForehand)
      _ <- Logger[F].debug(s"Player ${forehand.name} has meld : $marriage.")
    } yield state match {
      case _: EarlyGame => EarlyGameForehandTurn(updatedRound, ongoingMarriage = marriage.some)
      case _: LateGame => LateGameForehandTurn(updatedRound, ongoingMarriage = marriage.some)
    }
  }

  private def dealerTurn(state: DealerTurn, input: PlayCard): F[GameState] = {

    def resolveTurn(fhCard: Card, dlCard: Card): InternalGameState[F, Unit] = for {
      _      <- playCard(state.currentPlayer, dlCard)
      winner <- findWinner(fhCard, dlCard)
      _      <- winTurn(winner, fhCard, dlCard)
    } yield ()

    val drawCards: InternalGameState[F, Unit] = for {
      _ <- forehand[F] >>= drawCard
      _ <- dealer[F] >>= drawCard
    } yield ()

    def nextState(game: GameRound): F[GameState] = state match {
      case _: EarlyGameDealerTurn =>
        game.talon match {
          case _ :: Nil => finishEarlyGame(game)
          case _ => drawCards.runS(game).map(g => EarlyGameForehandTurn(g))
        }
      case _: LateGameDealerTurn =>
        game.forehand.hand match {
          case Nil => finishLateGame(game)
          case _ => F.pure(LateGameForehandTurn(game))
        }
    }

    for {
      _            <- checkPlayer(state.currentPlayer, input.playerId)
      dealerCard   <- checkPlayedCard(input.card, state.playableCards.contains(_))
      updatedRound <- resolveTurn(state.forehandCard, dealerCard).runS(state.round)
      nextState    <- nextState(updatedRound)
    } yield nextState
  }

  private def playCard(player: Player, card: Card): InternalGameState[F, Card] = StateT { state =>
    val (playedCard, remainingHand) = pickCard(card, player.hand)
    playedCard match {
      case None => Sync[F].raiseError(InvalidCard(s"Player ${player.name} has tried to play the card $card that he does not own."))
      case Some(c) =>
        val updatedPlayer = player.copy(hand = remainingHand)
        (state.updatePlayer(updatedPlayer), c).pure[F]
    }
  }

  private def findWinner(fhCard: Card, dlCard: Card): InternalGameState[F, Player] = StateT { state =>
    val winner =
      if ((dlCard.suit == fhCard.suit && dlCard.rank > fhCard.rank) || (dlCard.suit == state.trumpSuit))
        state.dealer
      else
        state.forehand
    (state, winner).pure[F]
  }

  private def winTurn(winner: Player, c1: Card, c2: Card): InternalGameState[F, Unit] = StateT { state =>
    val updatedScore = winner.score + c1.rank.value + c2.rank.value + winner.potentialMarriagePoints
    val updatedWonCards = winner.wonCards ++ List(c1, c2)
    val updatedWinner = winner.copy(wonCards = updatedWonCards, score = updatedScore)
    val updatedState = if (winner == state.forehand) {
      state.copy(forehand = updatedWinner)
    } else {
      state.copy(dealer = state.forehand, forehand = updatedWinner)
    }
    (updatedState, ()).pure[F]
  }

  private def drawCard(player: Player): InternalGameState[F, Card] = StateT { state =>
    for {
      (card, updatedDeck) <- drawFirstCardF[F](state.talon)
      _                   <- Logger[F].debug(s"Player ${player.name} has drawn $card")
      updatedPlayer = player.copy(hand = player.hand :+ card)
      updatedState = state.updatePlayer(updatedPlayer).copy(talon = updatedDeck)
    } yield (updatedState, card)
  }

  private def checkPlayer(expectedPlayer: Player, playerId: PlayerId): F[Unit] =
    if (expectedPlayer.id == playerId) F.unit else F.raiseError(WrongPlayer)

  private def checkPlayedCard(card: Card, playableRule: Card => Boolean): F[Card] =
    if (playableRule.apply(card)) card.pure[F] else F.raiseError(InvalidCard())

  private def finishEarlyGame(game: GameRound): F[GameState] = for {
    (lastCard, updatedDeck) <- drawFirstCardF[F](game.talon)
    updatedForehand = game.forehand.copy(hand = game.forehand.hand :+ lastCard)
    updatedDealer = game.dealer.copy(hand = game.dealer.hand :+ game.trumpCard)
    lateGame = game.copy(talon = updatedDeck, forehand = updatedForehand, dealer = updatedDealer)
    _ <- Logger[F].debug(s"Player ${updatedForehand.name} has drawn last card $lastCard")
    _ <- Logger[F].debug(s"Player ${updatedDealer.name} has drawn the trump card ${game.trumpCard}")
  } yield LateGameForehandTurn(lateGame)

  private def finishLateGame(game: GameRound): F[GameState] = for {
    _ <- Logger[F].debug("Last hand played with both player hands exhausted.")
    winner = game.forehand // forehand is the winner of the last trick
    loser = game.dealer
    outcome = TalonExhausted(winner.id, loser.id)
    updatedGame = game.copy(lastHandWonBy = winner.id.some)
  } yield applyRoundOutcome(updatedGame, outcome)

  private def claimVictory(state: ForehandTurn, input: ClaimVictory): F[GameState] = {
    val forehand = state.currentPlayer
    for {
      _ <- checkPlayer(forehand, input.playerId)
      _ <- Logger[F].debug(s"Player ${forehand.name} has claimed victory")
      outcome = findClaimedVictoryWinner(state)
      updatedRound = state.round.copy(victoryClaimedByForehand = true)
    } yield applyRoundOutcome(updatedRound, outcome)
  }

  private def findClaimedVictoryWinner(state: ForehandTurn): VictoryClaimed = {
    val forehand = state.currentPlayer
    val dealer = state.round.dealer
    val dealerScore = state.round.talonClosing match {
      case Some(tc) => tc.opponentScore
      case None => dealer.score
    }
    (forehand.score, dealerScore) match {
      case (fhs, 0) if fhs < 66 => VictoryClaimed(winner = dealer.id, loser = forehand.id, 3, successful = false)
      case (fhs, _) if fhs < 66 => VictoryClaimed(winner = dealer.id, loser = forehand.id, 2, successful = false)
      case (_, 0) => VictoryClaimed(winner = forehand.id, loser = dealer.id, 3)
      case (_, dls) if dls < 33 => VictoryClaimed(winner = forehand.id, loser = dealer.id, 2)
      case _ => VictoryClaimed(winner = forehand.id, loser = dealer.id, 1)
    }
  }

  private def applyRoundOutcome(round: GameRound, outcome: RoundOutcome): Finish = {
    val p1 = round.context.player1
    val p2 = round.context.player2
    val (up1, up2) = outcome.winner match {
      case p1.id => (p1.copy(score = p1.score - outcome.reward), p2)
      case p2.id => (p1, p2.copy(score = p2.score - outcome.reward))
    }
    val updatedContext = round.context.copy(player1 = up1, player2 = up2)
    val updatedRound = round.copy(context = updatedContext)
    Finish(updatedRound, outcome)
  }

  override def currentState: F[GameState] = fsm.getCurrentState
}
