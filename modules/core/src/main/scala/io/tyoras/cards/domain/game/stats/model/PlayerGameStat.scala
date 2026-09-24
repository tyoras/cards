package io.tyoras.cards.domain.game.stats.model

import io.chrisdavenport.fuuid.FUUID
import cats.Show
import cats.implicits.toShow
import io.github.iltotore.iron.*
import io.tyoras.cards.domain.game.GameType
import io.tyoras.cards.domain.game.Game
import io.tyoras.cards.domain.user.model.User

import java.time.ZonedDateTime

sealed trait PlayerGameStat extends Product with Serializable:
  protected type ThisType <: PlayerGameStat

  def playerId: User.ID
  def game: GameType
  def won: Game.Count
  def incrementWon: ThisType
  def draw: Game.Count
  def incrementDraw: ThisType
  def lost: Game.Count
  def incrementLost: ThisType
  def played: Game.Count        = (won + draw + lost).assume
  def winRate: Game.Percentage  = if played > 0 then ((won.toDouble / played.toDouble) * 100).assume else 0.0
  def loseRate: Game.Percentage = if played > 0 then ((lost.toDouble / played.toDouble) * 100).assume else 0.0

object PlayerGameStat:
  type ID = FUUID

  case class Data(playerId: User.ID, game: GameType, won: Game.Count, draw: Game.Count, lost: Game.Count) extends PlayerGameStat:
    override protected type ThisType = Data

    override def incrementWon: Data = copy(won = (won + 1).assume)

    override def incrementDraw: Data = copy(draw = (draw + 1).assume)

    override def incrementLost: Data = copy(lost = (lost + 1).assume)

  object Data:
    def apply(playerId: User.ID, game: GameType): Data = Data(playerId, game, 0, 0, 0)

    given Show[Data] = d => s"playerId = ${d.playerId} | game = ${d.game.label} | played = ${d.played} | won = ${d.won} | draw = ${d.draw} | lost = ${d.lost}"

  case class Existing(id: PlayerGameStat.ID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data) extends PlayerGameStat:
    override protected type ThisType = Existing

    override def playerId: User.ID       = data.playerId
    override def game: GameType          = data.game
    override def won: Game.Count         = data.won
    override def incrementWon: Existing  = copy(updatedAt = ZonedDateTime.now(), data = data.incrementWon)
    override def draw: Game.Count        = data.draw
    override def incrementDraw: Existing = copy(updatedAt = ZonedDateTime.now(), data = data.incrementDraw)
    override def lost: Game.Count        = data.lost
    override def incrementLost: Existing = copy(updatedAt = ZonedDateTime.now(), data = data.incrementLost)

  object Existing:
    given Show[Existing] = e => s"id = ${e.id} | created_at = ${e.createdAt} | updated_at = ${e.updatedAt} | ${e.data.show}"
