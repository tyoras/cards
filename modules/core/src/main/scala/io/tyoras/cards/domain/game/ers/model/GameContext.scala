package io.tyoras.cards.domain.game.ers.model

import cats.Show
import cats.syntax.all.*
import cats.data.NonEmptyList
import io.tyoras.cards.domain.card.{Card, Hand, Turn}
import io.tyoras.cards.domain.user.model.User

import scala.util.chaining.*
import java.time.ZonedDateTime

type PlayerId = User.ID

case class Player(id: PlayerId, hand: Hand = Nil):
  lazy val hasCards: Boolean = hand.nonEmpty
object Player:
  given Show[Player] = p => s"${p.id.show} ${p.hand.mkString_("[", "|", "]")}"

case class Elimination(playerId: PlayerId, turn: Turn)

case class GameContext(
    players: Map[PlayerId, Player],
    startedAt: ZonedDateTime,
    turnNumber: Turn,
    playerOrder: NonEmptyList[PlayerId],
    eliminations: List[Elimination] = List.empty
):
  override def toString: String = s"Egyptian Ratscrew game started at $startedAt | turn $turnNumber | players: [${players.values.toList.mkString_(", ")}]"

  def player(playerId: PlayerId): Option[Player] = players.get(playerId)

  def nextPlayerWithCards(currentPlayer: PlayerId): Option[PlayerId] =
    val order            = playerOrder.toList
    val idx              = order.indexOf(currentPlayer)
    val followingPlayers = order.drop(idx + 1) ++ order.take(idx + 1)
    followingPlayers.find(id => id != currentPlayer && player(id).fold(false)(_.hasCards))

  def pickFirstCard(playerId: PlayerId): Option[Card] = player(playerId).flatMap(_.hand.headOption)

  def updatePlayer(playerId: PlayerId)(update: Player => Player): GameContext =
    val updatedPlayers = players.updatedWith(playerId)(_.flatMap(update.andThen(Some(_))))
    copy(players = updatedPlayers)

  def eliminatePlayer(playerId: PlayerId): GameContext =
    copy(eliminations = Elimination(playerId, turnNumber) :: eliminations)

  def isEliminated(playerId: PlayerId): Boolean =
    eliminations.map(_.playerId).contains(playerId)

  def incrementTurnNumber: GameContext =
    copy(turnNumber = Turn.assume(turnNumber + 1))

  def playerHand(playerId: PlayerId): Hand = player(playerId).map(_.hand).getOrElse(Nil)

  lazy val playersCardCount: Map[PlayerId, Card.Count] = players.view.mapValues(_.hand.size.pipe(Card.Count.assume)).toMap

  lazy val allEliminated: Boolean = eliminations.size == players.size - 1
