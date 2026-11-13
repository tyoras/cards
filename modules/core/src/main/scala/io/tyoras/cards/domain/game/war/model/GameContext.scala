package io.tyoras.cards.domain.game.war.model

import io.tyoras.cards.domain.card.{Card, Hand, Turn}
import io.tyoras.cards.domain.user.model.User

import scala.util.chaining.*
import java.time.ZonedDateTime

type PlayerId = User.ID

case class Player(id: PlayerId, hand: Hand = Nil):
  lazy val eliminated: Boolean = hand.isEmpty

case class Elimination(playerId: PlayerId, turn: Turn)

case class GameContext(players: Map[PlayerId, Player], startedAt: ZonedDateTime, turnNumber: Turn, eliminations: List[Elimination] = List.empty):
  override def toString: String = s"War game started at $startedAt | turn $turnNumber | players: [${players.values.mkString(", ")}]"

  def player(playerId: PlayerId): Option[Player] = players.get(playerId)

  def pickFirstCard(playerId: PlayerId): Option[Card] = player(playerId).flatMap(_.hand.headOption)

  def updatePlayer(playerId: PlayerId)(update: Player => Player): GameContext =
    val updatedPlayers = players.updatedWith(playerId)(_.flatMap(update.andThen(Some(_))))
    copy(players = updatedPlayers)

  def eliminatePlayer(playerId: PlayerId): GameContext =
    copy(eliminations = Elimination(playerId, turnNumber) :: eliminations)

  def incrementTurnNumber: GameContext =
    copy(turnNumber = Turn.assume(turnNumber + 1))

  def playerHand(playerId: PlayerId): Hand = player(playerId).map(_.hand).getOrElse(Nil)

  lazy val playersCardCount: Map[PlayerId, Card.Count] = players.view.mapValues(_.hand.size.pipe(Card.Count.assume)).toMap

  lazy val allEliminated: Boolean = eliminations.size == players.size - 1
