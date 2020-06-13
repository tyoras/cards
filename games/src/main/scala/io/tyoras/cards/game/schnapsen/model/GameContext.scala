package io.tyoras.cards.game.schnapsen.model

import java.time.ZonedDateTime

import io.tyoras.cards.game.schnapsen.PlayerId

case class PlayerInfo(id: PlayerId, name: String, score: Int = 7)
object PlayerInfo {
  def reset(player: PlayerInfo): PlayerInfo =
    PlayerInfo(id = player.id, name = player.name)
}

case class GameContext(
  player1: PlayerInfo,
  player2: PlayerInfo,
  startedAt: ZonedDateTime,
  previousFirstDealer: Option[PlayerId] = None
) {
  private lazy val playersById: Map[PlayerId, PlayerInfo] = Map(player1.id -> player1, player2.id -> player2)

  def player(id: PlayerId): Either[SchnapsenError, PlayerInfo] =
    playersById.get(id).toRight[SchnapsenError](InvalidPlayer(s"Unknown player id ($id)"))
}
object GameContext {
  def reset(context: GameContext, startAt: ZonedDateTime): GameContext =
    GameContext(PlayerInfo.reset(context.player1), PlayerInfo.reset(context.player2), startAt)
}
