package io.tyoras.cards.domain

package object card:

  lazy val allSuits: Set[Suit]   = Set(Heart, Diamond, Club, Spade)
  lazy val blackSuits: Set[Suit] = allSuits.filter(_.color == Black)
  lazy val redSuits: Set[Suit]   = allSuits.filter(_.color == Red)

  lazy val defaultRanks: Set[Rank] = Set(Ace(), King(), Queen(), Jack(), Ten(), Nine(), Eight(), Seven(), Six(), Five(), Four(), Three(), Two())

  type Deck = List[Card]
  type Hand = List[Card]

  lazy val international52Deck: Deck = createDeck(allSuits, defaultRanks)

  def createDeck(suits: Set[Suit], ranks: Set[Rank]): Deck = (for
    s <- suits
    r <- ranks
  yield Card(s, r)).toList

  def shuffle(cards: Deck): Deck = util.Random.shuffle(cards)

  def pickCard(n: Int, hand: Hand): (Option[Card], Hand) = hand match
    case Nil                          => (None, hand)
    case _ if n < 0 || n >= hand.size => (None, hand)
    case _ =>
      val card          = hand(n)
      val remainingHand = hand.take(n) ++ hand.drop(n + 1)
      (Some(card), remainingHand)

  def pickCard(card: Card, hand: Hand): (Option[Card], Hand) =
    val index = hand.indexOf(card)
    pickCard(index, hand)

  def drawFirstCard(deck: Deck): (Option[Card], Deck) =
    val (h, d) = drawNCard(1, deck)
    h match
      case Nil => (None, d)
      case _   => (Some(h.head), d)

  def drawNCard(n: Int, deck: Deck): (Hand, Deck) =
    val takenCards    = deck.take(n)
    val remainingDeck = deck.drop(n)
    (takenCards, remainingDeck)
