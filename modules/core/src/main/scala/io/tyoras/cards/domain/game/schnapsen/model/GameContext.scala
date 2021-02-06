package io.tyoras.cards.domain.game.schnapsen.model

import java.time.ZonedDateTime

import io.tyoras.cards.domain.game.schnapsen.PlayerId

case class PlayerInfo(id: PlayerId, name: String, score: Int = 7) {
  lazy val reset: PlayerInfo = copy(score = 7)

  override def toString: String = s"$name score $score"
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

  override def toString: String = s"Started at: $startedAt - $player1 - $player2"
}
object GameContext {
  def reset(context: GameContext, startAt: ZonedDateTime): GameContext =
    GameContext(context.player1.reset, context.player2.reset, startAt)
}
