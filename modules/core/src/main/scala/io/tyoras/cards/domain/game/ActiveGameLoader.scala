package io.tyoras.cards.domain.game

import cats.effect.Async
import io.tyoras.cards.domain.game
import io.tyoras.cards.domain.game.war.War
import org.typelevel.log4cats.LoggerFactory
import cats.syntax.all.*

trait ActiveGameLoader[F[_]]:
  def loadFromState(gameId: Game.ID, gameType: GameType, gameState: gameType.State): F[ActiveGame[F, gameType.State, gameType.Input]]

object ActiveGameLoader:
  def make[F[_] : Async : LoggerFactory]: ActiveGameLoader[F] = new:
    def loadFromState(gameId: Game.ID, gameType: GameType, gameState: gameType.State): F[ActiveGame[F, gameType.State, gameType.Input]] =
      gameType match
        case GameTyp.Schnapsen => ??? // TODO implement Schnapsen.fromState[F](gameId, gameState.asInstanceOf[game.schnapsen.model.GameState])
        case GameTyp.War       =>
          War.fromState[F](gameId, gameState.asInstanceOf[game.war.model.GameState]).map(_.asInstanceOf[ActiveGame[F, gameType.State, gameType.Input]])
