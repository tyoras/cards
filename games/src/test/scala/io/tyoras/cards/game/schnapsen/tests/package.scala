package io.tyoras.cards.game.schnapsen

import java.time.{ZoneId, ZonedDateTime}

import cats.effect.IO
import cats.implicits.catsSyntaxOptionId
import io.chrisdavenport.fuuid.FUUIDGen
import io.tyoras.cards._
import io.tyoras.cards.game.schnapsen.model.{GameContext, Marriage, Player, PlayerInfo}
import org.scalacheck.Gen

package object tests {
  val suitGen: Gen[Suit] = Gen.oneOf(schnapsenSuits)
  val suitsGen: Gen[Set[Suit]] = Gen.containerOf[Set, Suit](suitGen)

  val rankGen: Gen[Rank] = Gen.oneOf(schnapsenRanks)
  val ranksGen: Gen[Set[Rank]] = Gen.containerOf[Set, Rank](rankGen)

  val deckGen: Gen[Deck] = Gen.delay(shuffle(baseDeck))
  val handGen: Gen[Hand] = for {
    n    <- Gen.chooseNum(0, 5)
    deck <- deckGen
  } yield deck.take(n)

  val cardGen: Gen[Card] = Gen.oneOf(baseDeck)

  def marriageGen(trumpSuit: Option[Suit] = None): Gen[Marriage] = for {
    suit <- suitGen
    king = Card(suit, King(4))
    queen = Card(suit, Queen(3))
    ts <- suitGen
    status = Marriage.Status.of(trumpSuit.getOrElse(ts), suit)
  } yield Marriage(king, queen, status)

  def marriagesGen(trumpSuit: Option[Suit] = None): Gen[List[Marriage]] = Gen.containerOf[Set, Marriage](marriageGen(trumpSuit)).map(_.toList)

  val playerIdGen: Gen[PlayerId] = Gen.delay(FUUIDGen[IO].random.unsafeRunSync())

  val playerGen: Gen[Player] = for {
    id        <- playerIdGen
    name      <- Gen.alphaNumStr
    hand      <- handGen
    score     <- Gen.chooseNum(0, Int.MaxValue)
    wonCards  <- handGen
    trumpSuit <- suitGen
    marriages <- marriagesGen(trumpSuit.some)
  } yield Player(id, name, hand, score, wonCards, marriages)

  val playerInfoGen: Gen[PlayerInfo] = for {
    id    <- playerIdGen
    name  <- Gen.alphaNumStr
    score <- Gen.chooseNum(0, 7)
  } yield PlayerInfo(id, name, score)

  val zonedDateTimeGen: Gen[ZonedDateTime] =
    Gen.calendar.map(cal => ZonedDateTime.ofInstant(cal.toInstant, ZoneId.systemDefault))

  val gameContextGen: Gen[GameContext] = for {
    player1 <- playerInfoGen
    player2 <- playerInfoGen
    startedAt <- zonedDateTimeGen
    previousFirstDealer <- Gen.option(Gen.oneOf(player1.id, player2.id))
  } yield GameContext(player1, player2, startedAt, previousFirstDealer)
}
