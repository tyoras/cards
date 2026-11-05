package io.tyoras.cards.tests.domain.game.schnapsen

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.game.GameTyp
import io.tyoras.cards.domain.game.schnapsen.model.*
import io.tyoras.cards.domain.game.schnapsen.model.Marriage.Status.{Common, Royal}
import io.tyoras.cards.domain.game.schnapsen.model.RoundOutcome.*
import io.tyoras.cards.domain.game.schnapsen.model.SchnapsenInput.GameInput.*
import io.tyoras.cards.domain.game.schnapsen.model.SchnapsenInput.MetaInput.*
import io.tyoras.cards.domain.game.schnapsen.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.tests.{gameIdGen, pick}
import io.tyoras.cards.util.fsm.concurrent.SynchronizedConcurrentFSM
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.{ZoneOffset, ZonedDateTime}
import java.util.UUID

class SchnapsenGameSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val logger = Slf4jLogger.getLogger[IO]

  private val dealerId: PlayerId   = User.ID(UUID.fromString("00000000-0000-0000-0000-000000000001"))
  private val forehandId: PlayerId = User.ID(UUID.fromString("00000000-0000-0000-0000-000000000002"))
  private val strangerId: PlayerId = User.ID(UUID.fromString("00000000-0000-0000-0000-000000000003"))
  private val startedAt            = ZonedDateTime.of(2026, 1, 1, 12, 0, 0, 0, ZoneOffset.UTC)
  private val context              = GameContext(PlayerInfo(dealerId, "Dealer"), PlayerInfo(forehandId, "Forehand"), startedAt)

  private def ace(s: Suit): Card  = Card(s, Ace(Card.Value(11)))
  private def ten(s: Suit): Card  = Card(s, Ten())
  private def jack(s: Suit): Card = Card(s, Jack(Card.Value(2)))

  // the trump suit is Heart unless stated otherwise
  private def round(
      dealerHand: Hand = Nil,
      forehandHand: Hand = Nil,
      talon: Deck = Nil,
      trump: Card = ace(Heart),
      dealerScore: Int = 0,
      forehandScore: Int = 0,
      dealerWon: Hand = Nil,
      forehandWon: Hand = Nil,
      forehandMarriages: List[Marriage] = Nil,
      talonClosing: Option[TalonClosing] = None,
      ctx: GameContext = context
  ): GameRound =
    GameRound(
      ctx,
      Player(dealerId, "Dealer", dealerHand, dealerScore, dealerWon),
      Player(forehandId, "Forehand", forehandHand, forehandScore, forehandWon, forehandMarriages),
      talon,
      trump,
      talonClosing
    )

  private def gameFrom(state: GameState): Schnapsen[IO] =
    SynchronizedConcurrentFSM.create[IO, GameState](state).map(fsm => new SchnapsenImplem[IO](gameIdGen.pick, fsm)(logger)).unsafeRunSync()

  private def submit(state: GameState, input: SchnapsenInput): GameState =
    gameFrom(state).submitInput(input).unsafeRunSync()

  // submits an input that must be rejected with an error, and checks the game state did not move
  private def rejection(state: GameState, input: SchnapsenInput): Throwable =
    val game   = gameFrom(state)
    val result = game.submitInput(input).attempt.unsafeRunSync()
    game.currentState.unsafeRunSync() shouldBe state
    result.left.value

  private def newGame(ctx: GameContext = context): Schnapsen[IO] = Schnapsen[IO](ctx).unsafeRunSync()

  private def finished(state: GameState): Finish = state match
    case f: Finish => f
    case other     => fail(s"Expected Finish but got $other")

  // ---- game creation

  "Schnapsen.apply" should "start in the Init state" in {
    newGame().currentState.unsafeRunSync() shouldBe a[Init]
  }

  it should "deal 5 cards to each player, one trump card and leave 9 cards in the talon, all distinct" in {
    val r = newGame().currentState.unsafeRunSync().round
    r.dealer.hand should have size 5
    r.forehand.hand should have size 5
    r.talon should have size 9
    (r.trumpCard :: r.dealer.hand ::: r.forehand.hand ::: r.talon) should contain theSameElementsAs baseDeck
  }

  it should "pick both players and remember the first dealer when there is no previous dealer" in {
    // repeated because the first dealer is drawn at random, with draws between equal ranks
    (1 to 100).foreach { _ =>
      val r = newGame().currentState.unsafeRunSync().round
      Set(r.dealer.id, r.forehand.id) shouldBe Set(dealerId, forehandId)
      r.context.previousFirstDealer shouldBe Some(r.dealer.id)
    }
  }

  it should "pass the deal to player 2 when player 1 was the previous dealer" in {
    val r = newGame(context.copy(previousFirstDealer = Some(dealerId))).currentState.unsafeRunSync().round
    r.dealer.id shouldBe forehandId
    r.forehand.id shouldBe dealerId
  }

  it should "pass the deal to player 1 when player 2 was the previous dealer" in {
    val r = newGame(context.copy(previousFirstDealer = Some(forehandId))).currentState.unsafeRunSync().round
    r.dealer.id shouldBe dealerId
    r.forehand.id shouldBe forehandId
  }

  it should "fail with a DeckError when the previous dealer is not one of the players" in {
    Schnapsen[IO](context.copy(previousFirstDealer = Some(strangerId))).attempt.unsafeRunSync().left.value shouldBe a[DeckError]
  }

  it should "expose the game type, an id and both players" in {
    val game = newGame()
    game.gameType shouldBe GameTyp.Schnapsen
    game.gameId should not be newGame().gameId
    game.playerIds.unsafeRunSync().toList should contain theSameElementsAs List(dealerId, forehandId)
  }

  "isFinished" should "be false while the round is being played" in {
    gameFrom(EarlyGameForehandTurn(round())).isFinished.unsafeRunSync() shouldBe false
  }

  it should "be true once the round is finished" in {
    gameFrom(Finish(round(), TalonExhausted(forehandId, dealerId))).isFinished.unsafeRunSync() shouldBe true
  }

  it should "be true once a player has exited the game" in {
    val game = gameFrom(EarlyGameForehandTurn(round()))
    game.submitInput(End(dealerId)).unsafeRunSync() shouldBe Exit(round())
    game.isFinished.unsafeRunSync() shouldBe true
  }

  // ---- meta inputs

  "Start" should "move from Init to the forehand turn" in {
    val r = round()
    submit(Init(r), Start(forehandId)) shouldBe EarlyGameForehandTurn(r)
  }

  it should "deal a new round from Finish, keeping the game scores and passing the deal" in {
    val ctx = GameContext(PlayerInfo(dealerId, "Dealer", 5), PlayerInfo(forehandId, "Forehand", 6), startedAt, Some(dealerId))
    submit(Finish(round(ctx = ctx), TalonExhausted(dealerId, forehandId)), Start(dealerId)) match
      case EarlyGameForehandTurn(r, None) =>
        r.dealer.id shouldBe forehandId
        r.dealer.hand should have size 5
        r.context.player1.score shouldBe 5
        r.context.player2.score shouldBe 6
      case other => fail(s"Unexpected state $other")
  }

  "Restart" should "deal a new round with the game scores reset" in {
    val ctx = GameContext(PlayerInfo(dealerId, "Dealer", 2), PlayerInfo(forehandId, "Forehand", 3), startedAt, Some(dealerId))
    submit(EarlyGameForehandTurn(round(ctx = ctx)), Restart(dealerId)) match
      case EarlyGameForehandTurn(r, None) =>
        r.context.player1.score shouldBe 7
        r.context.player2.score shouldBe 7
        r.context.startedAt should not be startedAt
        r.dealer.hand should have size 5
      case other => fail(s"Unexpected state $other")
  }

  "End" should "exit the game from any state" in {
    val r = round()
    submit(Init(r), End(dealerId)) shouldBe Exit(r)
    submit(EarlyGameDealerTurn(r, ace(Club)), End(forehandId)) shouldBe Exit(r)
  }

  "An input with no meaning in the current state" should "be ignored" in {
    val r = round(forehandHand = List(ace(Club)), dealerHand = List(ace(Spade)))
    submit(Init(r), PlayCard(forehandId, ace(Club))) shouldBe Init(r)
    submit(EarlyGameDealerTurn(r, ace(Club)), CloseTalon(dealerId)) shouldBe EarlyGameDealerTurn(r, ace(Club))
    submit(LateGameForehandTurn(r), CloseTalon(forehandId)) shouldBe LateGameForehandTurn(r)
    submit(LateGameForehandTurn(r), ExchangeTrumpJack(forehandId)) shouldBe LateGameForehandTurn(r)
    val finish = Finish(r, TalonExhausted(forehandId, dealerId))
    submit(finish, PlayCard(forehandId, ace(Club))) shouldBe finish
  }

  // ---- forehand playing a card

  "PlayCard on a forehand turn" should "remove the card from the hand and give the turn to the dealer in early game" in {
    val r = round(forehandHand = List(ace(Club), jack(Spade)), dealerHand = List(ace(Spade)))
    submit(EarlyGameForehandTurn(r), PlayCard(forehandId, ace(Club))) shouldBe
      EarlyGameDealerTurn(r.copy(forehand = r.forehand.copy(hand = List(jack(Spade)))), ace(Club))
  }

  it should "give the turn to the dealer in late game" in {
    val r = round(forehandHand = List(ace(Club), jack(Spade)), dealerHand = List(ace(Spade)))
    submit(LateGameForehandTurn(r), PlayCard(forehandId, jack(Spade))) shouldBe
      LateGameDealerTurn(r.copy(forehand = r.forehand.copy(hand = List(ace(Club)))), jack(Spade))
  }

  it should "be ignored when it comes from the dealer" in {
    val state = EarlyGameForehandTurn(round(forehandHand = List(ace(Club)), dealerHand = List(ace(Spade))))
    submit(state, PlayCard(dealerId, ace(Spade))) shouldBe state
  }

  it should "be ignored when the player does not hold the card" in {
    val state = EarlyGameForehandTurn(round(forehandHand = List(ace(Club))))
    submit(state, PlayCard(forehandId, ace(Spade))) shouldBe state
  }

  it should "be limited to the king and queen of an ongoing marriage" in {
    val marriage = Marriage(Club, Common)
    val state    = EarlyGameForehandTurn(round(forehandHand = List(king(Club), queen(Club), ace(Spade))), ongoingMarriage = Some(marriage))
    submit(state, PlayCard(forehandId, ace(Spade))) shouldBe state
    submit(state, PlayCard(forehandId, king(Club))) shouldBe a[EarlyGameDealerTurn]
  }

  it should "be ignored when the announced marriage card is not in the hand" in {
    val state = EarlyGameForehandTurn(round(forehandHand = List(ace(Spade))), ongoingMarriage = Some(Marriage(Club, Common)))
    submit(state, PlayCard(forehandId, king(Club))) shouldBe state
  }

  // ---- dealer playing a card, resolving the trick

  private def trickFrom(
      forehandCard: Card,
      dealerCard: Card,
      trump: Card = ace(Heart),
      forehandHand: Hand = List(jack(Diamond)),
      dealerHand: Hand = List(ten(Diamond)),
      talon: Deck = List(ten(Spade), jack(Spade), queen(Spade))
  ): GameState =
    val r = round(forehandHand = forehandHand, dealerHand = dealerCard :: dealerHand, talon = talon, trump = trump)
    submit(EarlyGameDealerTurn(r, forehandCard), PlayCard(dealerId, dealerCard))

  "PlayCard on a dealer turn" should "let the forehand win with the higher card of the same suit, then draw" in {
    trickFrom(ace(Club), jack(Club)) shouldBe EarlyGameForehandTurn(
      round(
        forehandHand = List(jack(Diamond), ten(Spade)),
        dealerHand = List(ten(Diamond), jack(Spade)),
        talon = List(queen(Spade)),
        forehandScore = 13,
        forehandWon = List(ace(Club), jack(Club))
      )
    )
  }

  it should "let the dealer win with the higher card of the same suit, swap the roles and let the winner draw first" in {
    trickFrom(jack(Club), ace(Club)) match
      case EarlyGameForehandTurn(r, None) =>
        r.forehand.id shouldBe dealerId
        r.forehand.score shouldBe 13
        r.forehand.wonCards shouldBe List(jack(Club), ace(Club))
        r.forehand.hand shouldBe List(ten(Diamond), ten(Spade))
        r.dealer.id shouldBe forehandId
        r.dealer.hand shouldBe List(jack(Diamond), jack(Spade))
        r.talon shouldBe List(queen(Spade))
      case other => fail(s"Unexpected state $other")
  }

  it should "let the forehand win when the dealer does not follow suit and does not play a trump" in {
    trickFrom(jack(Club), ace(Spade)).round.forehand.id shouldBe forehandId
  }

  it should "let the dealer win with a trump against a card of another suit" in {
    trickFrom(ace(Club), jack(Heart)).round.forehand.id shouldBe dealerId
  }

  it should "let the forehand win with a trump against a card of another suit" in {
    trickFrom(jack(Heart), ace(Club)).round.forehand.id shouldBe forehandId
  }

  it should "be ignored when it comes from the forehand" in {
    val state = EarlyGameDealerTurn(round(forehandHand = List(jack(Spade)), dealerHand = List(ace(Spade))), ace(Club))
    submit(state, PlayCard(forehandId, jack(Spade))) shouldBe state
  }

  it should "be ignored when the dealer does not hold the card" in {
    val state = EarlyGameDealerTurn(round(dealerHand = List(ace(Spade))), ace(Club))
    submit(state, PlayCard(dealerId, jack(Spade))) shouldBe state
  }

  it should "move to the late game when only one card is left in the talon, the loser taking the trump card" in {
    trickFrom(ace(Club), jack(Club), talon = List(queen(Spade))) match
      case LateGameForehandTurn(r, None) =>
        r.forehand.hand shouldBe List(jack(Diamond), queen(Spade))
        r.dealer.hand shouldBe List(ten(Diamond), ace(Heart))
        r.talon shouldBe empty
      case other => fail(s"Unexpected state $other")
  }

  it should "force the dealer to follow the rules of the late game" in {
    val state = LateGameDealerTurn(round(dealerHand = List(jack(Club), ace(Club), ace(Spade)), forehandHand = List(jack(Diamond))), ten(Club))
    submit(state, PlayCard(dealerId, ace(Spade))) shouldBe state
    submit(state, PlayCard(dealerId, jack(Club))) shouldBe state
    submit(state, PlayCard(dealerId, ace(Club))) shouldBe a[LateGameForehandTurn]
  }

  it should "continue the late game while the players have cards" in {
    val r = round(dealerHand = List(jack(Club), ten(Diamond)), forehandHand = List(jack(Diamond)))
    submit(LateGameDealerTurn(r, ace(Club)), PlayCard(dealerId, jack(Club))) match
      case LateGameForehandTurn(next, None) =>
        next.forehand.score shouldBe 13
        next.talon shouldBe empty
      case other => fail(s"Unexpected state $other")
  }

  it should "finish the round when the last trick is played, the forehand taking the last trick" in {
    val state = finished(submit(LateGameDealerTurn(round(dealerHand = List(jack(Club))), ace(Club)), PlayCard(dealerId, jack(Club))))
    state.outcome shouldBe TalonExhausted(forehandId, dealerId)
    state.round.lastHandWonBy shouldBe Some(forehandId)
    state.round.context.player2.score shouldBe 6
    state.round.context.player1.score shouldBe 7
  }

  it should "finish the round when the last trick is played, the dealer taking the last trick" in {
    val state = finished(submit(LateGameDealerTurn(round(dealerHand = List(ace(Club))), jack(Club)), PlayCard(dealerId, ace(Club))))
    state.outcome shouldBe TalonExhausted(dealerId, forehandId)
    state.round.lastHandWonBy shouldBe Some(dealerId)
    state.round.context.player1.score shouldBe 6
    state.round.context.player2.score shouldBe 7
  }

  // ---- meld

  "Meld" should "record the marriage without any point when the forehand has not won a card yet" in {
    val r = round(forehandHand = List(king(Club), queen(Club), jack(Spade)))
    submit(EarlyGameForehandTurn(r), Meld(forehandId, Club)) match
      case EarlyGameForehandTurn(next, ongoing) =>
        ongoing shouldBe Some(Marriage(Club, Common))
        next.forehand.score shouldBe 0
        next.forehand.marriages shouldBe List(Marriage(Club, Common))
      case other => fail(s"Unexpected state $other")
  }

  it should "grant 20 points for a common marriage when the forehand has already won a card" in {
    val r = round(forehandHand = List(king(Club), queen(Club)), forehandWon = List(ace(Spade), jack(Spade)), forehandScore = 13)
    submit(EarlyGameForehandTurn(r), Meld(forehandId, Club)).round.forehand.score shouldBe 33
  }

  it should "grant 40 points for a royal marriage when the forehand has already won a card" in {
    val r = round(forehandHand = List(king(Heart), queen(Heart)), forehandWon = List(ace(Spade), jack(Spade)), forehandScore = 13)
    submit(EarlyGameForehandTurn(r), Meld(forehandId, Heart)) match
      case EarlyGameForehandTurn(next, ongoing) =>
        ongoing shouldBe Some(Marriage(Heart, Royal))
        next.forehand.score shouldBe 53
      case other => fail(s"Unexpected state $other")
  }

  it should "stay in the late game when melding in late game" in {
    val r = round(forehandHand = List(king(Club), queen(Club)))
    submit(LateGameForehandTurn(r), Meld(forehandId, Club)) match
      case LateGameForehandTurn(_, ongoing) => ongoing shouldBe Some(Marriage(Club, Common))
      case other                            => fail(s"Unexpected state $other")
  }

  it should "be rejected when it comes from the dealer" in {
    rejection(EarlyGameForehandTurn(round(dealerHand = List(king(Club), queen(Club)))), Meld(dealerId, Club)) shouldBe WrongPlayer
  }

  it should "be rejected when the forehand does not hold the king and the queen" in {
    val state = EarlyGameForehandTurn(round(forehandHand = List(ace(Spade)), forehandWon = List(ace(Club), jack(Club)), forehandScore = 13))
    rejection(state, Meld(forehandId, Diamond)) shouldBe a[InvalidAction]
  }

  it should "be rejected when the forehand holds only one card of the pair" in {
    rejection(EarlyGameForehandTurn(round(forehandHand = List(king(Club), queen(Spade)))), Meld(forehandId, Club)) shouldBe a[InvalidAction]
  }

  it should "be rejected when a marriage is already in progress" in {
    val hand  = List(king(Club), queen(Club))
    val state = EarlyGameForehandTurn(round(forehandHand = hand), ongoingMarriage = Some(Marriage(Club, Common)))
    rejection(state, Meld(forehandId, Club)) shouldBe a[InvalidAction]
  }

  // ---- trump jack exchange

  "ExchangeTrumpJack" should "swap the trump jack from the hand with the trump card" in {
    val r = round(forehandHand = List(jack(Heart), ace(Club)))
    submit(EarlyGameForehandTurn(r), ExchangeTrumpJack(forehandId)) shouldBe
      EarlyGameForehandTurn(r.copy(trumpCard = jack(Heart), forehand = r.forehand.copy(hand = List(ace(Club), ace(Heart)))))
  }

  it should "be rejected when the forehand does not hold the trump jack" in {
    rejection(EarlyGameForehandTurn(round(forehandHand = List(jack(Club)))), ExchangeTrumpJack(forehandId)) shouldBe a[InvalidAction]
  }

  it should "be rejected when it comes from the dealer" in {
    rejection(EarlyGameForehandTurn(round(dealerHand = List(jack(Heart)))), ExchangeTrumpJack(dealerId)) shouldBe WrongPlayer
  }

  // ---- talon closing

  "CloseTalon" should "move to the late game and remember the opponent score at closing time" in {
    val r = round(dealerScore = 12)
    submit(EarlyGameForehandTurn(r), CloseTalon(forehandId)) shouldBe
      LateGameForehandTurn(r.copy(talonClosing = Some(TalonClosing(forehandId, 12))))
  }

  it should "be rejected when it comes from the dealer" in {
    rejection(EarlyGameForehandTurn(round()), CloseTalon(dealerId)) shouldBe WrongPlayer
  }

  it should "be rejected during a marriage" in {
    rejection(EarlyGameForehandTurn(round(), ongoingMarriage = Some(Marriage(Club, Common))), CloseTalon(forehandId)) shouldBe a[InvalidAction]
  }

  // ---- claiming victory

  private def claim(forehandScore: Int, dealerScore: Int, talonClosing: Option[TalonClosing] = None): Finish =
    finished(
      submit(EarlyGameForehandTurn(round(forehandScore = forehandScore, dealerScore = dealerScore, talonClosing = talonClosing)), ClaimVictory(forehandId))
    )

  "ClaimVictory" should "give 3 points to the forehand with 66 points when the dealer has none" in {
    claim(66, 0).outcome shouldBe VictoryClaimed(forehandId, dealerId, 3)
  }

  it should "give 2 points to the forehand with 66 points when the dealer has less than 33" in {
    claim(66, 32).outcome shouldBe VictoryClaimed(forehandId, dealerId, 2)
  }

  it should "give 1 point to the forehand with 66 points when the dealer has 33 or more" in {
    claim(70, 33).outcome shouldBe VictoryClaimed(forehandId, dealerId, 1)
  }

  it should "give 3 points to the dealer on a wrong claim when he has no point" in {
    claim(65, 0).outcome shouldBe VictoryClaimed(dealerId, forehandId, 3, successful = false)
  }

  it should "give 2 points to the dealer on a wrong claim when he has some points" in {
    claim(65, 10).outcome shouldBe VictoryClaimed(dealerId, forehandId, 2, successful = false)
  }

  it should "use the opponent score at talon closing time instead of the current one" in {
    claim(66, 40, Some(TalonClosing(forehandId, 0))).outcome shouldBe VictoryClaimed(forehandId, dealerId, 3)
  }

  it should "subtract the reward from the game score of the winner and flag the claim" in {
    val state = claim(66, 0)
    state.round.victoryClaimedByForehand shouldBe true
    state.round.context.player2.score shouldBe 4
    state.round.context.player1.score shouldBe 7
  }

  it should "be accepted during the late game" in {
    submit(LateGameForehandTurn(round(forehandScore = 66)), ClaimVictory(forehandId)) shouldBe a[Finish]
  }

  it should "be rejected when it comes from the dealer" in {
    rejection(EarlyGameForehandTurn(round()), ClaimVictory(dealerId)) shouldBe WrongPlayer
  }

  // ---- playing from a real deal

  "A game created with Schnapsen.apply" should "be playable until the talon is exhausted" in {
    val game = newGame()
    val id   = game.currentState.unsafeRunSync().round.forehand.id
    game.submitInput(Start(id)).unsafeRunSync() shouldBe a[EarlyGameForehandTurn]

    def playOneTurn(): GameState =
      game.currentState.unsafeRunSync() match
        case s: PlayerTurn => game.submitInput(PlayCard(s.currentPlayer.id, s.playableCards.head)).unsafeRunSync()
        case s             => s

    // 10 tricks of 2 cards use the whole deck: the 5 of the early game then the 5 of the late game
    val states = List.fill(20)(playOneTurn())
    states.last shouldBe a[Finish]
    game.isFinished.unsafeRunSync() shouldBe true
  }
