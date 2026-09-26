package io.tyoras.cards.server.protocol.game

import io.tyoras.cards.domain.game.*
import io.tyoras.cards.domain.user.model.User

import scala.util.control.NoStackTrace

final case class ConnectedPlayer(gameId: Game.ID, gameType: GameType, playerId: User.ID, name: String)

enum ProtocolError(val code: String, msg: String) extends Exception(msg) with NoStackTrace:
  case GameAlreadyFinished(gameId: Game.ID)                    extends ProtocolError("game_already_finished", s"Game with id $gameId is already finished")
  case ActiveGameNotFound(gameId: Game.ID, gameType: GameType) extends ProtocolError("game_not_found", s"$gameType game with id $gameId not found")
  case PlayerDoesNotBelongToGame(playerId: User.ID, gameId: Game.ID, gameType: GameType)
      extends ProtocolError("player_does_not_belong_to_game", s"Player $playerId is not a player in $gameType game with id $gameId")
  case IllegalGameInput(actualPlayerId: User.ID, inputPlayerId: User.ID, gameId: Game.ID, gameType: GameType) extends ProtocolError(
        "illegal_game_input",
        s"Player $actualPlayerId has tried to submit an input as player $inputPlayerId in $gameType game with id $gameId"
      )

final case class Games[F[_]](activeGames: Map[(Game.ID, GameType), ActiveGame[F, ?, ?]]):
  def filtered(gameType: GameType): List[ActiveGame[F, gameType.State, gameType.Input]] =
    activeGames.view.filterKeys { case (_, gt) => gt == gameType }.values.toList.map(_.asInstanceOf[ActiveGame[F, gameType.State, gameType.Input]])

  def get(gameId: Game.ID, gameType: GameType): Option[ActiveGame[F, gameType.State, gameType.Input]] =
    activeGames.get((gameId, gameType)).map(_.asInstanceOf[ActiveGame[F, gameType.State, gameType.Input]])

  def upsert(activeGame: ActiveGame[F, ?, ?]): Games[F] =
    copy(activeGames = activeGames.updated(activeGame.gameId -> activeGame.gameType, activeGame))

  def removeGame(gameId: Game.ID, gameType: GameType): Games[F] =
    copy(activeGames = activeGames.removed(gameId -> gameType))

object Games:
  def empty[F[_]]: Games[F] = Games[F](activeGames = Map.empty)
