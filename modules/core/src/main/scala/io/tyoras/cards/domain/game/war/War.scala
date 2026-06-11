package io.tyoras.cards.domain.game.war

import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.Async
import cats.effect.kernel.{Clock, Sync}
import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.game.war.War.BattleResult.*
import io.tyoras.cards.domain.game.war.model.*
import cats.syntax.all.*
import io.tyoras.cards.util.fsm.concurrent.SynchronizedConcurrentFSM
import io.chrisdavenport.cats.effect.time.implicits.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.game.GameError.NoPlayersError
import io.tyoras.cards.domain.game.{ActiveGame, GameTyp}
import io.tyoras.cards.domain.game.war.model.WarInput.GameInput.*
import io.tyoras.cards.domain.game.war.model.GameState.*
import io.tyoras.cards.domain.game.war.model.WarInput.MetaInput.*
import io.tyoras.cards.util.collection.syntax.*
import io.tyoras.cards.util.fsm.FinalStateMachine
import io.tyoras.cards.util.logging.syntax.*
import org.typelevel.log4cats.LoggerFactory

trait War[F[_]] extends ActiveGame[F, GameState, WarInput]:
  override val gameType: GameTyp[GameState, WarInput] = GameTyp.War

object War:

  private def initGameContext[F[_] : Sync](playerIds: NonEmptyList[PlayerId]): F[GameContext] =
    for
      startAt <- Clock[F].getZonedDateTimeUTC
      deck    <- Sync[F].delay(warDeck.shuffled)
      hands   = deck.divideN(playerIds.size)
      players = playerIds.toList.zip(hands).map((id, hand) => id -> Player(id, hand)).toMap
    yield GameContext(players, startAt, Turn.firstTurn)

  def apply[F[_] : Async : LoggerFactory](playerIds: NonEmptyList[PlayerId]): F[War[F]] =
    for
      logger  <- LoggerFactory.create[F]
      context <- initGameContext(playerIds)
      _       <- logger.debug(s"Starting new War game with initial game context : $context")
      fsm     <- SynchronizedConcurrentFSM.create[F, GameState](Init(context))
    yield new WarFSM[F](fsm)

  def fromState[F[_] : Async : LoggerFactory](state: GameState): F[War[F]] =
    for
      logger <- LoggerFactory.create[F]
      _      <- logger.debug(s"Resuming War game from state : $state")
      fsm    <- SynchronizedConcurrentFSM.create[F, GameState](state)
    yield new WarFSM[F](fsm)

  private class WarFSM[F[_] : Async : LoggerFactory](fsm: FinalStateMachine[F, GameState]) extends War[F]:
    private val logger                             = LoggerFactory.getLogger
    override def currentState: F[GameState]        = fsm.getCurrentState
    override def isFinished: F[Boolean]            = currentState.map(_.isInstanceOf[Finish])
    override val playerIds: F[NonEmptyList[FUUID]] =
      currentState.map(_.context.players.keys.toList).flatMap(ids => Sync[F].fromOption(NonEmptyList.fromList(ids), NoPlayersError))

    override def submitInput(input: WarInput): F[GameState] = fsm.transition { s =>
      val logCtx = input.playerId.ctx(playerIdKey)
      logger.debug(logCtx)(s"Submitting input [$input] on current state : $s") >>
        menu
          .orElse(game)
          .applyOrElse(
            s -> input,
            _ => logger.debug(logCtx)(s"Ignoring wrong input [$input]").as(s)
          )
    }

    private def menu: PartialFunction[(GameState, WarInput), F[GameState]] =
      case (s, restart: Restart) =>
        logger.debug(restart.playerId.ctx(playerIdKey))("Player has asked to restart a new game") >>
          initGameContext(NonEmptyList.fromListUnsafe(s.context.players.keys.toList)).map(Init(_))
      case (s, end: End) =>
        logger.debug(end.playerId.ctx(playerIdKey))("Player has asked to exit the game").as(Exit(s.context))

    private def game: PartialFunction[(GameState, WarInput), F[GameState]] =
      case (s: Init, i: Ready)          => playerReady(s, i)
      case (s: BattleTurn, i: PlayCard) => playCard(s, i)
      case (s: WarTurn, i: PlayCard)    => playCard(s, i)
      case (s: PlayerWinTurn, i: Ready) => ackTurnWin(s, i)

    private def playerReady(state: Init, input: Ready): F[GameState] =
      for
        _ <- checkPlayer(state.notReady, input.playerId)
        readyPlayers = state.ready + input.playerId
        updated      = state.copy(ready = readyPlayers)
        nextState    = if updated.notReady.isEmpty then BattleTurn(state.context) else updated
        _ <- logger.debug(input.playerId.ctx(playerIdKey))("Player ready for next turn")
      yield nextState

    private def ackTurnWin(state: PlayerWinTurn, input: Ready): F[GameState] =
      for
        _ <- checkPlayer(state.notAcked, input.playerId)
        ackedPlayers = state.acked + input.playerId
        updated      = state.copy(acked = ackedPlayers)
        nextState <-
          if updated.notAcked.nonEmpty then updated.pure
          else if updated.context.allEliminated then
            updated.context.players.values.filter(!_.eliminated).map(_.id) match
              case winnerId :: Nil => Finish(updated.context, winnerId).pure
              case _               => InvalidState("Finish state without a unique winner").raiseError
          else BattleTurn(state.context).pure
        playerReadyLog = logger.debug(input.playerId.ctx(playerIdKey))("Player ready for next turn")
      yield nextState

    private def checkPlayer(expectedPlayers: Set[PlayerId], playerId: PlayerId): F[Unit] =
      WrongPlayer.raiseError.unlessA(expectedPlayers.contains(playerId))

    private def checkPlayedCard(input: PlayCard)(context: GameContext): F[Card] =
      Sync[F].fromOption(
        context.pickFirstCard(input.playerId).filter(_.id == input.cardId),
        InvalidCard(s"${input.playerId} has played an invalid card ${input.cardId}")
      )

    private def playCard(state: BattleTurn, input: PlayCard): F[GameState] =
      for
        _          <- checkPlayer(state.missingPlays, input.playerId)
        playedCard <- checkPlayedCard(input)(state.context)
        newCtx            = state.context.updatePlayer(input.playerId)(p => p.copy(hand = p.hand.tail))
        updatedBattleTurn = BattleTurn(newCtx, playedCards = state.playedCards.updated(input.playerId, playedCard))
        nextState         = if updatedBattleTurn.missingPlays.isEmpty then resolveBattle(updatedBattleTurn) else updatedBattleTurn
        _ <- logger.debug(input.playerId.ctx(playerIdKey))(s"Card played: $playedCard")
      yield nextState

    private def resolveBattle(battleTurn: BattleTurn): GameState =
      val highestCard = battleTurn.playedCards.values.max
      val winners     = battleTurn.playedCards.filter(_._2.value == highestCard.value).keySet.toNes
      winners.toList match
        case winnerId :: Nil =>
          val wonCards = battleTurn.playedCards.values.toList
          winTurn(winnerId, wonCards)(battleTurn.context)
        case _ => initWarTurn(battleTurn, winners)

    private def winTurn(winnerId: PlayerId, wonCards: List[Card])(context: GameContext): PlayerWinTurn =
      val allEliminated = context.players.values.toSet
        .filter(_.eliminated)
        .map(_.id) - winnerId // removing winner in case he does not have cards anymore before getting the ones he just won
      val alreadyEliminated = context.eliminations.map(_.playerId).toSet
      val newlyEliminated   = allEliminated.diff(alreadyEliminated)
      val updatedCtx        =
        newlyEliminated.foldLeft(context)(_.eliminatePlayer(_)).updatePlayer(winnerId)(p => p.copy(hand = p.hand ::: wonCards)).incrementTurnNumber
      PlayerWinTurn(updatedCtx, winnerId, wonCards.toSet, newlyEliminated)

    private def initWarTurn(battleTurn: BattleTurn, involvedPlayers: NonEmptySet[PlayerId]): GameState =
      val firstRound = WarTurn.BattleRound(
        involvedPlayers = battleTurn.playedCards.keySet.toNes,
        hiddenPlayedCards = Map.empty,
        fightingCards = battleTurn.playedCards
      )
      val warTurn = WarTurn(battleTurn.context, NonEmptyList.one(firstRound))
      initNewWarRound(warTurn, involvedPlayers)

    private def playCard(state: WarTurn, input: PlayCard): F[GameState] =
      for
        _          <- checkPlayer(state.missingPlays, input.playerId)
        playedCard <- checkPlayedCard(input)(state.context)
        newCtx         = state.context.updatePlayer(input.playerId)(p => p.copy(hand = p.hand.tail))
        updatedWarTurn = state.copy(context = newCtx).playCard(input.playerId, playedCard)
        nextState      = if updatedWarTurn.allCardPlayed then resolveWarRound(updatedWarTurn) else updatedWarTurn
        _ <- logger.debug(input.playerId.ctx(playerIdKey))(s"Card played: $playedCard")
      yield nextState

    private def resolveWarRound(warTurn: WarTurn): GameState =
      // in case both players don't have enough cards we use the hidden cards instead
      val fightingCards =
        if warTurn.currentRound.fightingCards.nonEmpty then warTurn.currentRound.fightingCards
        // in case does not happen to have hidden cards we draw cards from a second deck instead
        else if warTurn.currentRound.hiddenPlayedCards.nonEmpty then warTurn.currentRound.hiddenPlayedCards
        else warTurn.currentRound.involvedPlayers.foldLeft(Map.empty[PlayerId, Card])((cards, id) => cards.updated(id, warDeck.shuffled.head))
      val highestCard = fightingCards.values.max
      val winners     = fightingCards.filter(_._2.value == highestCard.value).keySet.toNes
      winners.toList match
        case winnerId :: Nil =>
          val wonCards = warTurn.heap.toList
          winTurn(winnerId, wonCards)(warTurn.context)
        case _ => initNewWarRound(warTurn, winners)

    private def initNewWarRound(warTurn: WarTurn, involvedPlayers: NonEmptySet[PlayerId]): GameState =
      val nextRound = WarTurn.BattleRound(
        involvedPlayers = involvedPlayers,
        hiddenPlayedCards = Map.empty,
        fightingCards = Map.empty
      )
      val newWarTurn = warTurn.copy(battles = warTurn.battles :+ nextRound)
      if newWarTurn.allCardPlayed then resolveWarRound(newWarTurn) else newWarTurn

  enum BattleResult:
    case Player1Wins(cards: List[Card])
    case Player2Wins(cards: List[Card])
    case Battle(cards: List[Card])
