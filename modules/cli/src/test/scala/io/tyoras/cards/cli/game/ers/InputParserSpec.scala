package io.tyoras.cards.cli.game.ers

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import io.tyoras.cards.cli.local.game.ers.ErsCliError.{InvalidInput, InvalidState}
import io.tyoras.cards.cli.local.game.ers.InputParser
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.card.{Card, Turn}
import io.tyoras.cards.domain.game.ers.model.*
import io.tyoras.cards.domain.game.ers.model.ERsGameState.*
import io.tyoras.cards.domain.game.ers.model.ERsInput.GameInput.{Discard, PlayCard, Ready, Slap}
import io.tyoras.cards.domain.game.ers.model.ERsInput.MetaInput.{End, Restart}
import io.tyoras.cards.domain.user.model.User
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.time.ZonedDateTime
import java.util.UUID

class InputParserSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val p1: PlayerId = User.ID(UUID.randomUUID)
  private val p2: PlayerId = User.ID(UUID.randomUUID)

  private val p1Card = Card(Spade, Ace())
  private val p2Card = Card(Diamond, Queen())

  private val context = GameContext(
    Map(p1 -> Player(p1, List(p1Card, Card(Heart, King()))), p2 -> Player(p2, List(p2Card))),
    ZonedDateTime.now(),
    Turn.firstTurn,
    NonEmptyList.of(p1, p2)
  )
  private val emptyHandContext = context.updatePlayer(p1)(_.copy(hand = Nil))

  private val pile          = NonEmptyList.of(Card(Club, Two()), Card(Club, Three()))
  private val playerTurn    = PlayerTurn(context, p1, pile.toList)
  private val challengeTurn = ChallengeTurn(context, p2, p1, 2, 2, pile)
  private val slapPenalty   = SlapPenalty(context, p1, pile, playerTurn)
  private val winTurn       = PlayerWinTurn(context, p1, pile, Set.empty, acked = Set(p2))

  private val parser = InputParser[IO]

  "InputParser.parse" should "return the meta inputs in any state" in {
    for
      quit    <- parser.parse(playerTurn, p1, "\\q")
      restart <- parser.parse(Finish(context, p1), p2, "\\r")
    yield
      quit shouldBe NonEmptyList.one(End(p1))
      restart shouldBe NonEmptyList.one(Restart(p2))
  }

  it should "return Ready inputs for all the not ready players in Init state" in
    parser.parse(Init(context, Set(p2)), p1, "").asserting(_ shouldBe NonEmptyList.one(Ready(p1)))

  it should "fail in Init state when every player is already ready" in
    parser.parse(Init(context, Set(p1, p2)), p1, "").assertThrows[InvalidState.type]

  it should "return a PlayCard input in PlayerTurn and ChallengeTurn states" in {
    for
      turn      <- parser.parse(playerTurn, p1, "")
      challenge <- parser.parse(challengeTurn, p1, "")
    yield
      turn shouldBe NonEmptyList.one(PlayCard(p1))
      challenge shouldBe NonEmptyList.one(PlayCard(p1))
  }

  it should "fail to play a card with an empty hand" in
    parser.parse(playerTurn.copy(context = emptyHandContext), p1, "").assertThrows[InvalidState.type] >>
    parser.parse(challengeTurn.copy(context = emptyHandContext), p1, "").assertThrows[InvalidState.type]

  it should "return a Slap input in PlayerTurn and ChallengeTurn states" in {
    for
      turn      <- parser.parse(playerTurn, p2, "s")
      challenge <- parser.parse(challengeTurn, p1, "s")
    yield
      turn shouldBe NonEmptyList.one(Slap(p2))
      challenge shouldBe NonEmptyList.one(Slap(p1))
  }

  it should "reject a Slap input in the other states" in {
    parser.parse(Init(context), p1, "s").assertThrows[InvalidInput.type] >>
      parser.parse(slapPenalty, p1, "s").assertThrows[InvalidInput.type] >>
      parser.parse(winTurn, p1, "s").assertThrows[InvalidInput.type]
  }

  it should "return a Discard input of the first card of the hand in SlapPenalty state" in
    parser.parse(slapPenalty, p1, "").asserting(_ shouldBe NonEmptyList.one(Discard(p1, p1Card.id)))

  it should "fail to discard a card with an empty hand" in
    parser.parse(slapPenalty.copy(context = emptyHandContext), p1, "").assertThrows[InvalidState.type]

  it should "return Ready inputs for all the not acked players in PlayerWinTurn state" in
    parser.parse(winTurn, p2, "").asserting(_ shouldBe NonEmptyList.one(Ready(p1)))

  it should "fail in PlayerWinTurn state when every player has already acked" in
    parser.parse(winTurn.copy(acked = Set(p1, p2)), p1, "").assertThrows[InvalidState.type]

  it should "reject any other input in Finish state" in
    parser.parse(Finish(context, p1), p1, "").assertThrows[InvalidInput.type]

  it should "fail in Exit state" in
    parser.parse(Exit(context), p1, "").assertThrows[InvalidState.type]
