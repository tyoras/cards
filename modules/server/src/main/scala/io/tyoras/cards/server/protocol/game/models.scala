package io.tyoras.cards.server.protocol.game

import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.game.GameType
import io.tyoras.cards.domain.game.war.War

import scala.util.control.NoStackTrace

final case class ConnectedPlayer(gameId: FUUID, playerId: FUUID, name: String)

enum ProtocolError(val code: String, msg: String) extends Exception(msg) with NoStackTrace:
  case ActiveGameNotFound(gameId: FUUID, gameType: GameType[?, ?]) extends ProtocolError("game_not_found", s"$gameType game with id $gameId not found")
  case PlayerDoesNotBelongToGame(playerId: FUUID, gameId: FUUID, gameType: GameType[?, ?])
      extends ProtocolError("player_does_not_belong_to_game", s"Player $playerId is not a player in $gameType game with id $gameId")
  case IllegalGameInput(actualPlayerId: FUUID, inputPlayerId: FUUID, gameId: FUUID, gameType: GameType[?, ?]) extends ProtocolError(
        "illegal_game_input",
        s"Player $actualPlayerId has tried to submit an input as player $inputPlayerId in $gameType game with id $gameId"
      )

final case class Games[F[_]](warGames: Map[FUUID, War[F]])
object Games:
  def empty[F[_]]: Games[F] = Games[F](warGames = Map.empty)
