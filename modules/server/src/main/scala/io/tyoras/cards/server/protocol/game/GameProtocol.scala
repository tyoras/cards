package io.tyoras.cards.server.protocol.game

import cats.effect.Sync
import cats.effect.kernel.Ref
import cats.syntax.all.*
import dev.profunktor.auth.jwt.JwtToken
import io.chrisdavenport.fuuid.FUUID
import io.circe.Encoder
import io.circe.syntax.*
import io.tyoras.cards.domain.auth.{AuthError, AuthService}
import io.tyoras.cards.domain.game.war.War
import io.tyoras.cards.domain.game.*
import io.tyoras.cards.domain.user.User
import io.tyoras.cards.shared.protocol.game.OutputMessage
import io.tyoras.cards.shared.protocol.game.OutputMessage.{PlayerConnectionSuccess, PlayerDisconnected}
import org.typelevel.log4cats.LoggerFactory

trait GameProtocol[F[_]]:
  def currentState: F[Games[F]]
  def connect(gameId: FUUID, gameType: GameType, player: User.Existing): F[OutputMessage]
  def auth(gameId: FUUID, gameType: GameType, jwt: JwtToken): F[OutputMessage]
  def registerActiveGame(gameId: FUUID, game: ActiveGame[F, ?, ?]): F[Unit]
  def submitInput[State : Encoder, Input <: GameInput](gameId: FUUID, gameType: GameTyp[State, Input], playerId: FUUID, input: Input): F[List[OutputMessage]]
  def disconnect(playerRef: Ref[F, Option[ConnectedPlayer]]): F[OutputMessage]

object GameProtocol:
  def make[F[_] : Sync : LoggerFactory](authService: AuthService[F]): F[GameProtocol[F]] =
    Ref.of(Games.empty[F]).map { gamesRef =>
      new GameProtocol[F]:
        private val logger                     = LoggerFactory.getLogger
        override def currentState: F[Games[F]] = gamesRef.get

        override def connect(gameId: FUUID, gameType: GameType, player: User.Existing): F[OutputMessage] =
          (for
            games   <- currentState
            game    <- findActiveGame(gameId, gameType, games)
            players <- game.playerIds
            _       <- ProtocolError.PlayerDoesNotBelongToGame(player.id, gameId, gameType).raiseError.unlessA(players.exists(_ == player.id))
          yield PlayerConnectionSuccess(gameId, player.id, player.data.name)).handleError {
            case e: (ProtocolError.PlayerDoesNotBelongToGame | ProtocolError.ActiveGameNotFound) => OutputMessage.AuthError(e.code, e.getMessage)
          }

        private def findActiveGame[S, I <: GameInput](gameId: FUUID, gameType: GameTyp[S, I], games: Games[F]): F[ActiveGame[F, S, I]] =
          val game: Option[ActiveGame[F, S, I]] = gameType match
            case GameTyp.War => games.warGames.get(gameId)
            case _           => None // game is not supported yet
          Sync[F].fromOption(game, ProtocolError.ActiveGameNotFound(gameId, gameType))

        override def auth(gameId: FUUID, gameType: GameType, jwt: JwtToken): F[OutputMessage] =
          authService.authenticate(jwt).flatMap(connect(gameId, gameType, _)).handleError {
            case e: AuthError => OutputMessage.AuthError("token_auth", e.message)
            case _            => OutputMessage.AuthError("unexpected", "unexpected auth error")
          }

        override def registerActiveGame(gameId: FUUID, game: ActiveGame[F, ?, ?]): F[Unit] =
          gamesRef.update { games =>
            game.gameType match {
              case GameTyp.War =>
                val warGame = game.asInstanceOf[War[F]]
                games.copy(warGames = games.warGames.updated(gameId, warGame))
              case _ => games
            }
          }

        override def submitInput[State : Encoder, Input <: GameInput](
            gameId: FUUID,
            gameType: GameTyp[State, Input],
            playerId: FUUID,
            input: Input
        ): F[List[OutputMessage]] =
          (for
            _         <- checkInputPlayer(expectedPlayerId = playerId, input.playerId, gameId, gameType)
            games     <- currentState
            game      <- findActiveGame(gameId, gameType, games)
            players   <- game.playerIds
            gameState <- game.submitInput(input)
            // TODO filter context by user ??
            output = players.toList.map(OutputMessage.GameState(gameId, _, gameState.asJson))
          yield output).handleError {
            case e: (ProtocolError.IllegalGameInput | ProtocolError.ActiveGameNotFound) =>
              List(OutputMessage.ProtocolError(gameId, playerId, e.code, e.getMessage))
            case e => List(OutputMessage.GameError(gameId, playerId, "input_submission_failure", e.getMessage))
          }

        private def checkInputPlayer(expectedPlayerId: FUUID, inputPlayerId: FUUID, gameId: FUUID, gameType: GameType): F[Unit] =
          ProtocolError.IllegalGameInput(expectedPlayerId, inputPlayerId, gameId, gameType).raiseError.unlessA(expectedPlayerId == inputPlayerId)

        override def disconnect(playerRef: Ref[F, Option[ConnectedPlayer]]): F[OutputMessage] = {
          playerRef
            .modify {
              case Some(player) => None -> PlayerDisconnected(player.gameId, player.playerId, player.name)
              case None         => None -> OutputMessage.DiscardMessage
            }
            .flatTap {
              case PlayerDisconnected(gameId, playerId, playerName) => logger.info(s"Player $playerName [id=$playerId] disconnected from game $gameId")
              case _                                                => logger.warn("Disconnection on an already disconnected player")
            }
        }
    }
