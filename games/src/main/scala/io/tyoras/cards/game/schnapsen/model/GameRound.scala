package io.tyoras.cards.game.schnapsen.model

import io.tyoras.cards.game.schnapsen.PlayerId
import io.tyoras.cards.{Card, Deck, Hand, King, Queen, Suit}

sealed trait Role
case object Forehand extends Role
case object Dealer extends Role

case class Player(id: PlayerId, name: String, hand: Hand = Nil, score: Int = 0, wonCards: Hand = Nil, marriages: List[Marriage] = Nil) {
  override def toString = s"name = $name ($id) \t| score = $score \t| hand = ${hand.mkString(" ")}"
  lazy val potentialMarriagePoints: Int = if (wonCards.nonEmpty) marriages.foldRight(0)(_.status.score + _) else 0
  lazy val hasFailedMarriage: Boolean = wonCards.isEmpty && marriages.nonEmpty
}

case class GameRound(
  context: GameContext,
  dealer: Player,
  forehand: Player,
  talon: Deck,
  trumpCard: Card,
  talonClosing: Option[TalonClosing] = None,
  lastHandWonBy: Option[PlayerId] = None,
  victoryClaimedByForehand: Boolean = false
) {
  override def toString: String =
    s"""
       |\tContext\t$context
       |\tDealer\t\t$dealer
       |\tForehand\t$forehand
       |\tTrump card\t$trumpCard
       |\tTalon\t\t${if (talon.isEmpty) "empty" else talon.mkString(" ")}\t| status = ${talonClosing.fold("open")(p =>
      s"closed by ${playersById(p.closedBy).name}"
    )}""".stripMargin

  val trumpSuit: Suit = trumpCard.suit
  val roles: Map[Role, Player] = Map(Forehand -> forehand, Dealer -> dealer)
  val playersById: Map[PlayerId, Player] = Map(forehand.id -> forehand, dealer.id -> dealer)

  def updatePlayer(updatedPlayer: Player): GameRound =
    if (updatedPlayer.id == dealer.id)
      copy(dealer = updatedPlayer)
    else
      copy(forehand = updatedPlayer)
}

case class Marriage(king: Card, queen: Card, status: Marriage.Status) {
  override def toString: String = s"$status marriage between $king and $queen"
}
object Marriage {
  def apply(suit: Suit, status: Status = Common): Marriage =
    Marriage(Card(suit, King(4)), Card(suit, Queen(3)), status)

  trait Status {
    def score: Int
  }
  object Status {
    def of(trumpSuit: Suit, suit: Suit): Status =
      if (suit == trumpSuit) Royal else Common
  }
  case object Royal extends Status {
    override val score = 40
  }
  case object Common extends Status {
    override val score = 20
  }
}

case class TalonClosing(closedBy: PlayerId, opponentScore: Int)

sealed trait RoundOutcome {
  def winner: PlayerId

  def loser: PlayerId

  def reward: Int
}
case class VictoryClaimed(override val winner: PlayerId, override val loser: PlayerId, override val reward: Int, successful: Boolean = true)
  extends RoundOutcome
case class TalonExhausted(override val winner: PlayerId, override val loser: PlayerId) extends RoundOutcome {
  override val reward: Int = 1
}
