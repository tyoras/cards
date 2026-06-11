package io.tyoras.cards.server.protocol.game

import cats.effect.{Async, Resource}
import cats.effect.kernel.Ref
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import dev.profunktor.auth.jwt.JwtToken
import io.chrisdavenport.fuuid.FUUID
import io.circe.*
import io.circe.syntax.*
import io.tyoras.cards.domain.auth.AuthService
import io.tyoras.cards.domain.auth.model.AuthError
import io.tyoras.cards.domain.game.war.War
import io.tyoras.cards.domain.game.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.protocol.game.OutputMessage
import io.tyoras.cards.shared.protocol.game.OutputMessage.{PlayerConnectionSuccess, PlayerDisconnected}
import org.typelevel.log4cats.LoggerFactory
import io.tyoras.cards.domain.game.war.codecs.given

trait GameProtocol[F[_]]:
  def activeGames: F[Games[F]]
  def connectPlayer(gameId: FUUID, gameType: GameType, player: User.Existing): F[OutputMessage]
  def authPlayer(gameId: FUUID, gameType: GameType, jwt: JwtToken): F[OutputMessage]
  def registerActiveGame(gameId: FUUID, game: ActiveGame[F, ?, ?]): F[Unit]
  def currentState[State](gameId: FUUID, gameType: GameTyp[State, ?], playerId: FUUID): F[OutputMessage]
  def submitInput[State, Input <: GameInput](gameId: FUUID, gameType: GameTyp[State, Input], playerId: FUUID, input: Input): F[List[OutputMessage]]
  def endGame(gameId: FUUID, gameType: GameType): F[List[OutputMessage]]
  def disconnect(playerRef: Ref[F, Option[ConnectedPlayer]]): F[OutputMessage]

object GameProtocol:
  def make[F[_] : Async : LoggerFactory](authService: AuthService[F], gameService: GameService[F]): Resource[F, GameProtocol[F]] =
    Resource.make(AtomicCell[F].of(Games.empty[F]).map { gamesRef =>
      new GameProtocol[F]:
        private val logger = LoggerFactory.getLogger

        override def activeGames: F[Games[F]] = gamesRef.get

        override def connectPlayer(gameId: FUUID, gameType: GameType, player: User.Existing): F[OutputMessage] =
          (for
            game    <- gamesRef.evalModify(findActiveGame(gameId, gameType, _))
            players <- game.playerIds
            _       <- ProtocolError.PlayerDoesNotBelongToGame(player.id, gameId, gameType).raiseError.unlessA(players.exists(_ == player.id))
          yield PlayerConnectionSuccess(gameId, gameType, player.id, player.data.name)).handleError {
            case e: (ProtocolError.PlayerDoesNotBelongToGame | ProtocolError.ActiveGameNotFound) => OutputMessage.AuthError(e.code, e.getMessage)
          }

        private def findActiveGame[S, I <: GameInput](gameId: FUUID, gameType: GameTyp[S, I], games: Games[F]): F[(Games[F], ActiveGame[F, S, I])] =
          for
            game <- gameType match
              case GameTyp.War =>
                games.warGames
                  .get(gameId)
                  .fold(
                    for
                      gameData <- findActiveGameData[war.model.GameState](gameId)
                      found    <- gameData.traverse(data => War.fromState[F](data.state).map(_.asInstanceOf[ActiveGame[F, S, I]]))
                    yield found.map(data => updateActiveGames(games, gameId, data) -> data)
                  )(game => (games, game).some.pure)
              case _ => none.pure // game is not supported yet
            found <- Async[F].fromOption(game, ProtocolError.ActiveGameNotFound(gameId, gameType))
          yield found

        private def findActiveGameData[S : Decoder](gameId: FUUID): F[Option[Game.Existing[S]]] =
          for
            found <- gameService.readById[S](gameId)
            _     <- Async[F].raiseError(ProtocolError.GameAlreadyFinished(gameId)).unlessA(found.exists(_.data.finishedAt.isEmpty))
          yield found

        override def authPlayer(gameId: FUUID, gameType: GameType, jwt: JwtToken): F[OutputMessage] =
          authService.authenticate(jwt).flatMap(connectPlayer(gameId, gameType, _)).handleError {
            case e: AuthError => OutputMessage.AuthError("token_auth", e.message)
            case _            => OutputMessage.AuthError("unexpected", "unexpected auth error")
          }

        override def registerActiveGame(gameId: FUUID, game: ActiveGame[F, ?, ?]): F[Unit] =
          gamesRef.update(updateActiveGames(_, gameId, game))

        private def updateActiveGames(games: Games[F], gameId: FUUID, game: ActiveGame[F, ?, ?]): Games[F] =
          game.gameType match
            case GameTyp.War =>
              val warGame = game.asInstanceOf[War[F]]
              games.copy(warGames = games.warGames.updated(gameId, warGame))
            case _ => games

        override def currentState[State](gameId: FUUID, gameType: GameTyp[State, ?], playerId: FUUID): F[OutputMessage] =
          import gameType.given
          (for
            game      <- gamesRef.evalModify(games => findActiveGame(gameId, gameType, games))
            gameState <- game.currentState
            playerGameState: gameType.PlayerState = gameState.filterForPlayer(playerId)
          yield OutputMessage.GameState(gameId, playerId, playerGameState.asJson)).handleError {
            case e: ProtocolError.ActiveGameNotFound =>
              OutputMessage.ProtocolError(gameId, playerId, e.code, e.getMessage)
            case e => OutputMessage.GameError(gameId, playerId, "state_failure", e.getMessage)
          }

        override def submitInput[State, Input <: GameInput](
            gameId: FUUID,
            gameType: GameTyp[State, Input],
            playerId: FUUID,
            input: Input
        ): F[List[OutputMessage]] =
          import gameType.given
          (for
            _           <- checkInputPlayer(expectedPlayerId = playerId, input.playerId, gameId, gameType)
            game        <- gamesRef.evalModify(findActiveGame(gameId, gameType, _))
            players     <- game.playerIds
            gameState   <- game.submitInput(input)
            endMessages <- game.isFinished.flatMap(finished => if finished then endGame(gameId, gameType) else Nil.pure)
            output = players.toList.map { recipient =>
              val playerGameState: gameType.PlayerState = gameState.filterForPlayer(recipient)
              OutputMessage.GameState(gameId, recipient, playerGameState.asJson)
            }
          yield output ::: endMessages).handleError {
            case e: (ProtocolError.IllegalGameInput | ProtocolError.ActiveGameNotFound) =>
              List(OutputMessage.ProtocolError(gameId, playerId, e.code, e.getMessage))
            case e => List(OutputMessage.GameError(gameId, playerId, "input_submission_failure", e.getMessage))
          }

        private def checkInputPlayer(expectedPlayerId: FUUID, inputPlayerId: FUUID, gameId: FUUID, gameType: GameType): F[Unit] =
          ProtocolError.IllegalGameInput(expectedPlayerId, inputPlayerId, gameId, gameType).raiseError.unlessA(expectedPlayerId == inputPlayerId)

        override def endGame(gameId: FUUID, gameType: GameType): F[List[OutputMessage]] =
          for output <- gameType match
            case GameTyp.War =>
              for output <- gamesRef.evalModify(games =>
                  for
                    activeGame   <- findActiveGame(gameId, GameTyp.War, games).map(_._2)
                    currentState <- activeGame.currentState
                    found        <- findActiveGameData[war.model.GameState](gameId)
                    gameData     <- Async[F].fromOption(found, ProtocolError.ActiveGameNotFound(gameId, gameType))
                    _            <- gameService.update(gameData.withUpdatedState(currentState))
                  yield games.copy(warGames = games.warGames - gameId) -> List(OutputMessage.GameEnded(gameId))
                )
              yield output
            case _ => Nil.pure // game is not supported yet
          yield output

        override def disconnect(playerRef: Ref[F, Option[ConnectedPlayer]]): F[OutputMessage] = {
          playerRef
            .modify {
              case Some(player) => None -> PlayerDisconnected(player.gameId, player.playerId, player.name)
              case None         => None -> OutputMessage.DiscardMessage
            }
            .flatTap {
              case PlayerDisconnected(gameId, playerId, playerName) =>
                gamesRef.evalUpdate(games =>
                  for
                    activeGame   <- findActiveGame(gameId, GameTyp.War, games).map(_._2)
                    currentState <- activeGame.currentState
                    found        <- findActiveGameData[war.model.GameState](gameId)
                    gameData     <- Async[F].fromOption(found, ProtocolError.ActiveGameNotFound(gameId, activeGame.gameType))
                    _            <- gameService.update(gameData.withUpdatedState(currentState))
                    _            <- logger.info(s"Persisted game state for game $gameId after player $playerName [id=$playerId] disconnection")
                  yield games
                ) <* logger.info(s"Player $playerName [id=$playerId] disconnected from game $gameId")

              case _ => logger.warn("Disconnection on an already disconnected player")
            }
        }
    })(protocol =>
      protocol.activeGames.flatMap(_.warGames.toList.traverse_ { (id, game) =>
        protocol.endGame(id, game.gameType)
      })
    )
