package io.tyoras.cards.game.schnapsen

import java.time.{ZoneId, ZonedDateTime}

import cats.effect.IO
import cats.implicits.catsSyntaxOptionId
import io.chrisdavenport.fuuid.FUUIDGen
import io.tyoras.cards._
import io.tyoras.cards.game.schnapsen.model._
import org.scalacheck.{Arbitrary, Gen}

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
    ts <- trumpSuit.fold(suitGen)(ts => Gen.const(ts))
    status = Marriage.Status.of(trumpSuit.getOrElse(ts), suit)
  } yield Marriage(king, queen, status)

  def marriagesGen(trumpSuit: Option[Suit] = None): Gen[List[Marriage]] = Gen.containerOf[Set, Marriage](marriageGen(trumpSuit)).map(_.toList)

  val playerIdGen: Gen[PlayerId] = Gen.delay(FUUIDGen[IO].random.unsafeRunSync())

  def playerGen(id: Option[PlayerId] = None, name: Option[String] = None, trumpSuit: Option[Suit] = None): Gen[Player] = for {
    pid       <- id.fold(playerIdGen)(pid => Gen.const(pid))
    pname     <- name.fold(Gen.alphaNumStr)(pname => Gen.const(pname))
    hand      <- handGen
    score     <- Gen.chooseNum(0, Int.MaxValue)
    wonCards  <- handGen
    ts        <- trumpSuit.fold(suitGen)(ts => Gen.const(ts))
    marriages <- marriagesGen(ts.some)
  } yield Player(pid, pname, hand, score, wonCards, marriages)

  val playerInfoGen: Gen[PlayerInfo] = for {
    id    <- playerIdGen
    name  <- Gen.alphaNumStr
    score <- Gen.chooseNum(0, 7)
  } yield PlayerInfo(id, name, score)

  val zonedDateTimeGen: Gen[ZonedDateTime] =
    Gen.calendar.map(cal => ZonedDateTime.ofInstant(cal.toInstant, ZoneId.systemDefault))

  val gameContextGen: Gen[GameContext] = for {
    player1             <- playerInfoGen
    player2             <- playerInfoGen
    startedAt           <- zonedDateTimeGen
    previousFirstDealer <- Gen.option(Gen.oneOf(player1.id, player2.id))
  } yield GameContext(player1, player2, startedAt, previousFirstDealer)

  def talonClosingGen(playerIds: Seq[PlayerId]): Gen[TalonClosing] = for {
    closedBy      <- Gen.oneOf(playerIds)
    opponentScore <- Gen.posNum[Int]
  } yield TalonClosing(closedBy, opponentScore)

  //FIXME complete and coherent gen for GameRound
  val gameRoundGen: Gen[GameRound] = for {
    context   <- gameContextGen
    deck      <- deckGen
    trumpCard <- Gen.oneOf(deck)
    (_, d2) = pickCard(trumpCard, deck)
    p1 <- playerGen(context.player1.id.some, context.player1.name.some, trumpCard.suit.some)
    p2 <- playerGen(context.player2.id.some, context.player2.name.some, trumpCard.suit.some)
    playerIds = Seq(p1.id, p2.id)
    victoryClaimedByForehand <- Arbitrary.arbitrary[Boolean]
    talonClosing             <- Gen.option(talonClosingGen(playerIds))
    lastHandWonBy            <- Gen.option(Gen.oneOf(playerIds))
  } yield GameRound(context, dealer = p1, forehand = p2, d2, trumpCard, talonClosing, lastHandWonBy, victoryClaimedByForehand)
}
