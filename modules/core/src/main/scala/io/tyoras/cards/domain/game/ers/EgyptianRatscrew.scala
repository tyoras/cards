package io.tyoras.cards.domain.game.ers

import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.kernel.{Clock, Sync}
import cats.effect.std.{SecureRandom, UUIDGen}
import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.game.ers.model.*
import cats.syntax.all.*
import io.tyoras.cards.util.fsm.concurrent.SynchronizedConcurrentFSM
import io.chrisdavenport.cats.effect.time.implicits.*
import io.tyoras.cards.domain.game.GameError.NoPlayersError
import io.tyoras.cards.domain.game.{ActiveGame, Game, GameTyp}
import io.tyoras.cards.domain.game.ers.model.ERSInput.GameInput.*
import io.tyoras.cards.domain.game.ers.model.ERsGameState.*
import io.tyoras.cards.domain.game.ers.model.ERSInput.MetaInput.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.util.fsm.FinalStateMachine
import io.tyoras.cards.util.logging.syntax.*
import org.typelevel.log4cats.LoggerFactory

trait EgyptianRatscrew[F[_]] extends ActiveGame[F, ERsGameState, ERSInput]:
  override val gameType: GameTyp[ERsGameState, ERSInput] = GameTyp.EgyptianRatscrew

object EgyptianRatscrew:

  private def initGameContext[F[_] : Sync](playerIds: NonEmptyList[PlayerId]): F[GameContext] =
    for
      startAt <- Clock[F].getZonedDateTimeUTC
      deck    <- Sync[F].delay(ersDeck.shuffled)
      hands   = deck.divideN(playerIds.size)
      players = playerIds.toList.zip(hands).map((id, hand) => id -> Player(id, hand)).toMap
    yield GameContext(players, startAt, Turn.firstTurn, playerIds)

  def apply[F[_] : Async : LoggerFactory](playerIds: NonEmptyList[PlayerId]): F[EgyptianRatscrew[F]] =
    for
      logger                <- LoggerFactory.create[F]
      given SecureRandom[F] <- SecureRandom.javaSecuritySecureRandom
      context               <- initGameContext(playerIds)
      id                    <- UUIDGen[F].randomUUID.map(Game.ID(_))
      _                     <- logger.debug(s"Starting new Egyptian Ratscrew game ($id) with initial game context : $context")
      fsm                   <- SynchronizedConcurrentFSM.create[F, ERsGameState](Init(context))
    yield new EgyptianRatscrewFSM[F](id, fsm)

  def fromState[F[_] : Async : LoggerFactory](id: Game.ID, state: ERsGameState): F[EgyptianRatscrew[F]] =
    for
      logger <- LoggerFactory.create[F]
      _      <- logger.debug(s"Resuming Egyptian ratscrew game from state : $state")
      fsm    <- SynchronizedConcurrentFSM.create[F, ERsGameState](state)
    yield new EgyptianRatscrewFSM[F](id, fsm)

  private class EgyptianRatscrewFSM[F[_] : Async : LoggerFactory](id: Game.ID, fsm: FinalStateMachine[F, ERsGameState]) extends EgyptianRatscrew[F]:
    private val logger                               = LoggerFactory.getLogger
    override val gameId: Game.ID                     = id
    override def currentState: F[ERsGameState]       = fsm.getCurrentState
    override def isFinished: F[Boolean]              = currentState.map(s => s.isInstanceOf[Finish] || s.isInstanceOf[Exit])
    override val playerIds: F[NonEmptyList[User.ID]] =
      currentState.map(_.context.players.keys.toList).flatMap(ids => Sync[F].fromOption(NonEmptyList.fromList(ids), NoPlayersError))

    override def submitInput(input: ERSInput): F[ERsGameState] = fsm.transition { s =>
      val logCtx = input.playerId.ctx(playerIdKey)
      logger.debug(logCtx)(s"Submitting input [$input] on current state : $s") >>
        menu
          .orElse(game)
          .applyOrElse(
            s -> input,
            _ => logger.debug(logCtx)(s"Ignoring wrong input [$input]").as(s)
          )
    }

    private def menu: PartialFunction[(ERsGameState, ERSInput), F[ERsGameState]] =
      case (s, restart: Restart) =>
        logger.debug(restart.playerId.ctx(playerIdKey))("Player has asked to restart a new game") >>
          initGameContext(NonEmptyList.fromListUnsafe(s.context.players.keys.toList)).map(Init(_))
      case (s, end: End) =>
        logger.debug(end.playerId.ctx(playerIdKey))("Player has asked to exit the game").as(Exit(s.context))

    private def game: PartialFunction[(ERsGameState, ERSInput), F[ERsGameState]] =
      case (s: Init, i: Ready)             => playerReady(s, i)
      case (s: PlayerTurn, i: PlayCard)    => playCard(s, i)
      case (s: PlayerTurn, i: Slap)        => slap(s, i)
      case (s: ChallengeTurn, i: PlayCard) => playCard(s, i)
      case (s: ChallengeTurn, i: Slap)     => slap(s, i)
      case (s: SlapPenalty, i: Ready)      => ackPenalty(s, i)
      case (s: SlapPenalty, i: PlayCard)   => discardCard(s, i)
      case (s: PlayerWinTurn, i: Ready)    => ackTurnWin(s, i)

    private def playerReady(state: Init, input: Ready): F[ERsGameState] =
      for
        _ <- checkPlayer(state.notReady, input.playerId)
        readyPlayers = state.ready + input.playerId
        updated      = state.copy(ready = readyPlayers)
        nextState    = if updated.notReady.isEmpty then PlayerTurn(state.context, state.context.playerOrder.head) else updated
        _ <- logger.debug(input.playerId.ctx(playerIdKey))("Player ready for next turn")
      yield nextState

    private def ackTurnWin(state: PlayerWinTurn, input: Ready): F[ERsGameState] =
      for
        _ <- checkPlayer(state.notAcked, input.playerId)
        ackedPlayers = state.acked + input.playerId
        updated      = state.copy(acked = ackedPlayers)
        nextState <-
          if updated.notAcked.nonEmpty then updated.pure
          else if updated.context.allEliminated then
            updated.context.players.values.filter(_.hasCards).map(_.id) match
              case winnerId :: Nil => Finish(updated.context, winnerId).pure
              case _               => InvalidState("Finish state without a unique winner").raiseError
          else PlayerTurn(state.context.incrementTurnNumber, state.winnerId).pure
        _ <- logger.debug(input.playerId.ctx(playerIdKey))(s"Player ${input.playerId} acked player ${state.winnerId} turn win")
      yield nextState

    private def checkPlayer(expectedPlayers: Set[PlayerId], playerId: PlayerId): F[Unit] =
      WrongPlayer.raiseError.unlessA(expectedPlayers.contains(playerId))

    private def checkPlayedCard(input: PlayCard)(context: GameContext): F[Card] =
      Sync[F].fromOption(
        context.pickFirstCard(input.playerId).filter(_.id == input.cardId),
        InvalidCard(s"${input.playerId} has played an invalid card ${input.cardId}")
      )

    private def playCard(state: PlayerTurn, input: PlayCard): F[ERsGameState] =
      val currentPlayerId = input.playerId
      for
        _          <- checkPlayer(Set(state.player), currentPlayerId)
        playedCard <- checkPlayedCard(input)(state.context)
        newCtx    = state.context.updatePlayer(currentPlayerId)(p => p.copy(hand = p.hand.tail))
        newHeap   = NonEmptyList(playedCard, state.pile)
        nextState = playCardNextState(newCtx, newHeap, currentPlayerId, playedCard)
        _ <- logger.debug(input.playerId.ctx(playerIdKey))(s"Card played: $playedCard")
      yield nextState

    private def playCardNextState(newCtx: GameContext, heap: NonEmptyList[Card], currentPlayerId: PlayerId, playedCard: Card): ERsGameState =
      newCtx.nextPlayerWithCards(currentPlayerId) match
        case Some(nextPlayerId) if playedCard.rank > Rank.Ten() =>
          val challenge = headCardChallenge(playedCard.rank)
          ChallengeTurn(newCtx, currentPlayerId, nextPlayerId, challenge, challenge, heap)
        case Some(nextPlayerId) =>
          PlayerTurn(newCtx, nextPlayerId, heap.toList)
        case None =>
          winTurn(newCtx, currentPlayerId, heap)

    private def playCard(state: ChallengeTurn, input: PlayCard): F[ERsGameState] =
      val currentPlayerId = input.playerId
      for
        _          <- checkPlayer(Set(state.challenger), currentPlayerId)
        playedCard <- checkPlayedCard(input)(state.context)
        newCtx         = state.context.updatePlayer(currentPlayerId)(p => p.copy(hand = p.hand.tail))
        newHeap        = playedCard :: state.pile
        remainingTries = state.remainingTries - 1
        nextState      = playCardNextState(state, newCtx, newHeap, remainingTries, currentPlayerId, playedCard)
        _ <- logger.debug(input.playerId.ctx(playerIdKey))(s"Card played: $playedCard")
      yield nextState

    private def playCardNextState(
        state: ChallengeTurn,
        newCtx: GameContext,
        newHeap: NonEmptyList[Card],
        remainingTries: Int,
        currentPlayerId: PlayerId,
        playedCard: Card
    ): ERsGameState =
      newCtx.nextPlayerWithCards(currentPlayerId) match
        case Some(nextPlayerId) if playedCard.rank > Rank.Ten() =>
          val newChallenge = headCardChallenge(playedCard.rank)
          ChallengeTurn(newCtx, owner = state.challenger, challenger = nextPlayerId, totalTries = newChallenge, remainingTries = newChallenge, pile = newHeap)
        case Some(nextPlayerId) if remainingTries > 0 =>
          val challenger = if newCtx.player(state.challenger).fold(false)(_.hasCards) then state.challenger else nextPlayerId
          state.copy(context = newCtx, challenger = challenger, remainingTries = remainingTries, pile = newHeap)
        case _ => // no more players with cards or no more retry left
          winTurn(newCtx, state.owner, newHeap)

    private def winTurn(context: GameContext, winnerId: PlayerId, wonCards: NonEmptyList[Card]): PlayerWinTurn =
      val withWonCards     = context.updatePlayer(winnerId)(p => p.copy(hand = p.hand ::: wonCards.reverse.toList))
      val turnEliminations = withWonCards.playersCardCount.filter((pId, c) => c == Card.Count(0) && !withWonCards.isEliminated(pId)).keySet
      val updatedCtx       = turnEliminations.foldLeft(withWonCards)(_.eliminatePlayer(_))
      PlayerWinTurn(updatedCtx, winnerId, wonCards, turnEliminations)

    private def slap(state: PlayerTurn, input: Slap): F[ERsGameState] =
      NonEmptyList.fromList(state.pile) match
        case Some(pile) => slap(state, pile, input.playerId).pure
        case None       => InvalidAction(s"Player [${input.playerId}] tried to slap an empty pile").raiseError

    private def slap(state: ChallengeTurn, input: Slap): F[ERsGameState] =
      slap(state, state.pile, input.playerId).pure

    private def slap(state: PlayerTurn | ChallengeTurn, pile: NonEmptyList[Card], playerId: PlayerId): ERsGameState =
      if validateSlap(pile) then winTurn(state.context, playerId, pile)
      else SlapPenalty(state.context, playerId, pile, state)

    private def validateSlap(pile: NonEmptyList[Card]): Boolean =
      val topCardsRanks = pile.take(2).map(_.rank)
      topCardsRanks.size == 2 && topCardsRanks.head == topCardsRanks.last

    private def ackPenalty(state: SlapPenalty, input: Ready): F[ERsGameState] =
      for
        _ <- checkPlayer(state.notAcked, input.playerId)
        ackedPlayers = state.acked + input.playerId
        updated      = state.copy(acked = ackedPlayers)
        _ <- logger.debug(input.playerId.ctx(playerIdKey))(s"Player ${input.playerId} acked player ${state.player} slap penalty")
      yield penaltyNextState(updated)

    private def discardCard(state: SlapPenalty, input: PlayCard): F[ERsGameState] =
      for
        _             <- checkPlayer(Set(state.player), input.playerId)
        discardedCard <- checkPlayedCard(input)(state.context)
        _             <- InvalidAction(s"Player [${state.player}] has discarded a card but penalty is already fulfilled").raiseError.whenA(state.penaltyPayed)
        newCtx  = state.context.updatePlayer(input.playerId)(p => p.copy(hand = p.hand.tail))
        updated = state.copy(context = newCtx, discardedCards = state.discardedCards :+ discardedCard)
      yield penaltyNextState(updated)

    private def penaltyNextState(state: SlapPenalty): ERsGameState =
      if state.notAcked.nonEmpty || !state.penaltyPayed then state
      else
        state.originalState match
          case s: PlayerTurn    => s.copy(context = state.context, pile = s.pile ::: state.discardedCards)
          case s: ChallengeTurn => s.copy(context = state.context, pile = s.pile ++ state.discardedCards)
