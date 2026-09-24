package io.tyoras.cards.domain.game.stats.model

import cats.data.NonEmptyList
import io.github.iltotore.iron.*
import io.tyoras.cards.domain.game.Game
import io.tyoras.cards.domain.user.model.User

case class PlayerGameStats(playerId: User.ID, stats: NonEmptyList[PlayerGameStat.Existing]):
  lazy val totalPlayed: Game.Count        = stats.map(_.played).foldLeft(0)(_ + _).assume
  lazy val totalWon: Game.Count           = stats.map(_.won).foldLeft(0)(_ + _).assume
  lazy val totalLost: Game.Count          = stats.map(_.lost).foldLeft(0)(_ + _).assume
  lazy val totalWinRate: Game.Percentage  = if totalPlayed > 0 then ((totalWon.toDouble / totalPlayed.toDouble) * 100).assume else 0.0
  lazy val totalLoseRate: Game.Percentage = if totalPlayed > 0 then ((totalLost.toDouble / totalPlayed.toDouble) * 100).assume else 0.0
