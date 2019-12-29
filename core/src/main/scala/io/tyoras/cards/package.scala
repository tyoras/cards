package io.tyoras

package object cards {

  abstract class GameError(val code: String, msg: String) extends Exception(msg)

  val allSuits: List[Suit] = List(Heart, Diamond, Club, Spade)
  val blackSuits: List[Suit] = allSuits.filter(_.color == Black)
  val redSuits: List[Suit] = allSuits.filter(_.color == Red)

  val defaultRanks: List[Rank] = List(Ace(), King(), Queen(), Jack(), Ten(), Nine(), Eight(), Seven(), Six(), Five(), Four(), Three(), Two())

  type Deck = List[Card]
  type Hand = List[Card]

  val international52Deck: Deck = createDeck(allSuits, defaultRanks)

  def createDeck(suits: List[Suit], ranks: List[Rank]): Deck = for {
    s <- suits
    r <- ranks
  } yield Card(s, r)

  def shuffle(cards: Deck): Deck = util.Random.shuffle(cards)

  def randomPick[A](list: List[A]): A = util.Random.shuffle(list).head

  def pickCard(n: Int, hand: Hand): (Card, Hand) = {
    val card = hand(n)
    val remainingHand = if (n < 0 || hand.size < n) hand else hand.take(n) ++ hand.drop(n + 1)
    (card, remainingHand)
  }

  def pickCard(card: Card, hand: Hand): Hand = hand.filterNot(_ == card)

  def takeFirstCard(deck: Deck): (Card, Deck) = {
    val (h, d) = takeNCard(1, deck)
    (h.head, d)
  }

  def takeNCard(n: Int, deck: Deck): (Hand, Deck) = {
    val takenCards = deck.take(n)
    val remainingDeck = deck.drop(n)
    (takenCards, remainingDeck)
  }

}
