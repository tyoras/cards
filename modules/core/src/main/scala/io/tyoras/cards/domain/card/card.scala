package io.tyoras.cards.domain.card

import Rank.*
import Color.*
import Suit.*

lazy val allSuits: Set[Suit]   = Set(Heart, Diamond, Club, Spade)
lazy val blackSuits: Set[Suit] = allSuits.filter(_.color == Black)
lazy val redSuits: Set[Suit]   = allSuits.filter(_.color == Red)

lazy val defaultRanks: Set[Rank] = Set(Ace(), King(), Queen(), Jack(), Ten(), Nine(), Eight(), Seven(), Six(), Five(), Four(), Three(), Two())

type CardValue = Int
type Deck      = List[Card]
type Hand      = List[Card]

object Hand:
  val empty: Hand = List.empty

lazy val international52Deck: Deck = Deck.create(allSuits, defaultRanks)

object Deck:
  def create(suits: Set[Suit], ranks: Set[Rank]): Deck = (for
    s <- suits
    r <- ranks
  yield Card(s, r)).toList

extension (cards: List[Card])
  def shuffled: Deck = util.Random.shuffle(cards)

  def drawFirstCard: (Option[Card], Deck) =
    val (h, d) = drawNCard(1)
    h match
      case Nil => (None, d)
      case _   => (Some(h.head), d)

  def drawNCard(n: Int): (Hand, Deck) =
    val takenCards    = cards.take(n)
    val remainingDeck = cards.drop(n)
    (takenCards, remainingDeck)

  def divideN(n: Int): Iterator[Hand] = cards.grouped(cards.length / n)

  def pickCard(n: Int): (Option[Card], Hand) =
    cards match
      case Nil                           => (None, cards)
      case _ if n < 0 || n >= cards.size => (None, cards)
      case _ =>
        val card          = cards(n)
        val remainingHand = cards.take(n) ++ cards.drop(n + 1)
        (Some(card), remainingHand)

  def pickCard(card: Card): (Option[Card], Hand) =
    val index = cards.indexOf(card)
    pickCard(index)
