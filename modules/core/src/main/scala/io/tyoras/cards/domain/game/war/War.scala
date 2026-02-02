package io.tyoras.cards.domain.game.war

import io.tyoras.cards.domain.card.*

import scala.annotation.tailrec

object War:

  def divide(cards: Deck): (Hand, Hand) = cards.splitAt(cards.length / 2)

  sealed trait BattleResult
  case class Player1Wins(cards: List[Card]) extends BattleResult
  case class Player2Wins(cards: List[Card]) extends BattleResult
  case class War(cards: List[Card])         extends BattleResult

  def score(player1Card: Card, player2Card: Card, previousTurnCards: List[Card] = List()): BattleResult =
    player1Card.rank.value - player2Card.rank.value match
      case s if s == 0 => War(player1Card :: player2Card :: previousTurnCards)
      case s if s > 0  => Player1Wins(player1Card :: player2Card :: previousTurnCards)
      case s if s < 0  => Player2Wins(player2Card :: player1Card :: previousTurnCards)

  @tailrec
  def battle(player1Hand: Hand, player2Hand: Hand, previousTurnCards: List[Card] = List()): (Hand, Hand) =
    (player1Hand, player2Hand) match
      case h @ (_, Nil) => h
      case h @ (Nil, _) => h
      case (nextCard1 :: remainingCards1, nextCard2 :: remainingCards2) =>
        score(nextCard1, nextCard2, previousTurnCards) match
          case Player1Wins(cards) => (remainingCards1 ::: cards, remainingCards2)
          case Player2Wins(cards) => (remainingCards1, remainingCards2 ::: cards)
          case War(cards)         => battle(remainingCards1, remainingCards2, cards)

  @tailrec
  def play(hands: (Hand, Hand)): (Hand, Hand) = hands match
    case (h, Nil)                       => (h, Nil)
    case (Nil, h)                       => (Nil, h)
    case (player1: Hand, player2: Hand) => play(battle(player1, player2))
