package io.tyoras.cards.server.protocol.game

import io.tyoras.cards.domain.game.{Game, GameTyp, GameType}
import io.tyoras.cards.domain.game.war.War
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

final case class Games[F[_]](warGames: Map[Game.ID, War[F]]):
  def removeGame(gameType: GameType, gameId: Game.ID): Games[F] =
    gameType match
      case GameTyp.War => copy(warGames = warGames - gameId)
      case _           => this
object Games:
  def empty[F[_]]: Games[F] = Games[F](warGames = Map.empty)
