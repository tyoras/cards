package io.tyoras.cards.tests.domain.game.schnapsen

import io.circe.parser.decode
import io.circe.syntax.*
import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.game.GameStateFilter
import io.tyoras.cards.domain.game.schnapsen.codecs.given
import io.tyoras.cards.domain.game.schnapsen.model.Marriage.Status.{Common, Royal}
import io.tyoras.cards.domain.game.schnapsen.model.RoundOutcome.{TalonExhausted, VictoryClaimed}
import io.tyoras.cards.domain.game.schnapsen.model.SchnapsenInput.GameInput.*
import io.tyoras.cards.domain.game.schnapsen.model.SchnapsenInput.MetaInput.*
import io.tyoras.cards.domain.game.schnapsen.model.*
import io.tyoras.cards.domain.game.schnapsen.model.PlayerGameState.given
import io.tyoras.cards.domain.game.schnapsen.{PlayerId, king, queen}
import io.tyoras.cards.tests.{pick, userIdGen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{ZoneOffset, ZonedDateTime}

class SchnapsenGameStateSpec extends AnyFlatSpec with Matchers:

  private val dealerId: PlayerId   = userIdGen.pick
  private val forehandId: PlayerId = userIdGen.pick
  private val context              = GameContext(
    PlayerInfo(dealerId, "Dealer"),
    PlayerInfo(forehandId, "Forehand"),
    ZonedDateTime.of(2026, 1, 1, 12, 0, 0, 0, ZoneOffset.UTC)
  )

  private def ace(s: Suit): Card  = Card(s, Ace(Card.Value(11)))
  private def ten(s: Suit): Card  = Card(s, Ten())
  private def jack(s: Suit): Card = Card(s, Jack(Card.Value(2)))

  // the trump suit is Heart
  private def round(dealerHand: Hand = Nil, forehandHand: Hand = Nil, talon: Deck = Nil, talonClosing: Option[TalonClosing] = None): GameRound =
    GameRound(context, Player(dealerId, "Dealer", dealerHand), Player(forehandId, "Forehand", forehandHand), talon, ace(Heart), talonClosing)

  "Init" should "have a label and no playable cards" in {
    val state = Init(round(forehandHand = List(ace(Club))))
    state.label shouldBe "Initialisation"
    state.playableCards shouldBe empty
    state.toString should include("Initialisation")
    state.toString should include("Playable cards \tNone")
  }

  "GameState.player" should "find the players of the game" in {
    val state = Init(round())
    state.player(dealerId) shouldBe Right(context.player1)
    state.player(userIdGen.pick).isLeft shouldBe true
  }

  "EarlyGameForehandTurn" should "have the forehand as current player, and its whole hand as playable cards" in {
    val state = EarlyGameForehandTurn(round(forehandHand = List(ace(Club), jack(Spade))))
    state.label shouldBe "Early game - forehand turn"
    state.currentPlayer.id shouldBe forehandId
    state.playableCards shouldBe List(ace(Club), jack(Spade))
    state.toString should include(ace(Club).toString)
  }

  it should "only allow to play the cards of an ongoing marriage" in {
    val state = EarlyGameForehandTurn(round(forehandHand = List(king(Club), queen(Club), jack(Spade))), Some(Marriage(Club, Common)))
    state.playableCards shouldBe List(king(Club), queen(Club))
  }

  it should "find the trump jack, and allow to exchange it only when holding it" in {
    EarlyGameForehandTurn(round()).trumpJack shouldBe jack(Heart)
    EarlyGameForehandTurn(round(forehandHand = List(jack(Heart)))).canExchangeTrumpJack shouldBe true
    EarlyGameForehandTurn(round(forehandHand = List(jack(Club)))).canExchangeTrumpJack shouldBe false
  }

  "ForehandTurn.possibleMarriages" should "be empty when the hand holds no pair of king and queen of the same suit" in {
    EarlyGameForehandTurn(round(forehandHand = List(ace(Club), king(Club), queen(Spade)))).possibleMarriages shouldBe empty
  }

  it should "find a common marriage" in {
    EarlyGameForehandTurn(round(forehandHand = List(king(Club), ace(Spade), queen(Club)))).possibleMarriages shouldBe List(Marriage(Club, Common))
  }

  it should "find a royal marriage in the trump suit" in {
    LateGameForehandTurn(round(forehandHand = List(queen(Heart), king(Heart)))).possibleMarriages shouldBe List(Marriage(Heart, Royal))
  }

  it should "find several marriages" in {
    val hand = List(king(Club), king(Heart), queen(Spade), queen(Club), king(Spade))
    EarlyGameForehandTurn(round(forehandHand = hand)).possibleMarriages should contain theSameElementsAs List(Marriage(Club, Common), Marriage(Spade, Common))
  }

  "EarlyGameDealerTurn" should "allow the dealer to play any card" in {
    val state = EarlyGameDealerTurn(round(dealerHand = List(jack(Club), ace(Spade))), ace(Diamond))
    state.label shouldBe "Early game - dealer turn"
    state.currentPlayer.id shouldBe dealerId
    state.playableCards shouldBe List(jack(Club), ace(Spade))
    state.toString should include(s"Forehand card\t${ace(Diamond)}")
  }

  "LateGameForehandTurn" should "have the forehand as current player, and its whole hand as playable cards" in {
    val state = LateGameForehandTurn(round(forehandHand = List(ace(Club))))
    state.label shouldBe "Late game - forehand turn"
    state.currentPlayer.id shouldBe forehandId
    state.playableCards shouldBe List(ace(Club))
  }

  it should "only allow to play the cards of an ongoing marriage" in {
    val state = LateGameForehandTurn(round(forehandHand = List(king(Club), queen(Club), jack(Spade))), Some(Marriage(Club, Common)))
    state.playableCards shouldBe List(king(Club), queen(Club))
  }

  "LateGameDealerTurn" should "force to head the trick when possible" in {
    val state = LateGameDealerTurn(round(dealerHand = List(jack(Club), ace(Club), ace(Heart), ace(Spade))), ten(Club))
    state.label shouldBe "Late game - dealer turn"
    state.currentPlayer.id shouldBe dealerId
    state.playableCards shouldBe List(ace(Club))
    state.toString should include(s"Forehand card\t${ten(Club)}")
  }

  it should "force to follow suit when the trick cannot be headed" in {
    LateGameDealerTurn(round(dealerHand = List(jack(Club), ace(Heart))), ten(Club)).playableCards shouldBe List(jack(Club))
  }

  it should "force to play a trump when the suit cannot be followed" in {
    LateGameDealerTurn(round(dealerHand = List(ace(Spade), jack(Heart))), ten(Club)).playableCards shouldBe List(jack(Heart))
  }

  it should "allow any card when neither the suit nor a trump can be played" in {
    LateGameDealerTurn(round(dealerHand = List(ace(Spade), jack(Diamond))), ten(Club)).playableCards shouldBe List(ace(Spade), jack(Diamond))
  }

  "Finish and Exit" should "have labels" in {
    Finish(round(), TalonExhausted(forehandId, dealerId)).label shouldBe "Finish"
    Exit(round()).label shouldBe "Exit"
  }

  "GameRound.toString" should "display an empty and open talon" in {
    val text = round().toString
    text should include("empty")
    text should include("status = open")
  }

  it should "display the talon cards and who closed it" in {
    val text =
      round(dealerHand = List(ace(Club)), forehandHand = List(ace(Spade)), talon = List(jack(Club)), talonClosing = Some(TalonClosing(forehandId, 0))).toString
    text should include(jack(Club).toString)
    text should include("closed by Forehand")
  }

  "Player" should "start with an empty hand by default" in {
    Player(dealerId, "Dealer").hand shouldBe empty
  }

  "PlayerInfo.toString" should "display the name and the score" in {
    context.player1.toString shouldBe "Dealer score 7"
  }

  "GameContext.toString" should "display the players" in {
    context.toString should include("Dealer score 7")
    context.toString should include("Forehand score 7")
  }

  "RoundOutcome" should "expose its reward, winner and loser" in {
    val claimed: VictoryClaimed = VictoryClaimed(forehandId, dealerId, 3)
    claimed.reward shouldBe 3
    claimed.winner shouldBe forehandId
    claimed.loser shouldBe dealerId
    claimed.successful shouldBe true
    val exhausted = TalonExhausted(dealerId, forehandId)
    exhausted.reward shouldBe 1
    exhausted.winner shouldBe dealerId
    exhausted.loser shouldBe forehandId
  }

  "SchnapsenError" should "expose its code and message" in {
    val errors = List(
      DeckError("deck")       -> "deck_error",
      WrongPlayer             -> "wrong_player",
      InvalidAction()         -> "invalid_action",
      InvalidCard()           -> "invalid_card",
      InvalidPlayer("player") -> "invalid_player"
    )
    errors.foreach { (error, code) =>
      error.code shouldBe code
      error.getMessage should not be empty
    }
  }

  "SchnapsenInput" should "be displayed by its label and expose the player" in {
    val inputs = List[(SchnapsenInput, String)](
      Start(dealerId)               -> "Start game",
      Restart(dealerId)             -> "Restart game",
      End(dealerId)                 -> "Quit game",
      PlayCard(dealerId, ace(Club)) -> s"Play card ${ace(Club)}",
      ExchangeTrumpJack(dealerId)   -> "Exchange trump jack",
      CloseTalon(dealerId)          -> "Close the talon",
      Meld(dealerId, Club)          -> s"Meld $Club King and $Club Queen",
      ClaimVictory(dealerId)        -> "Claim victory"
    )
    inputs.foreach { (input, label) =>
      input.label shouldBe label
      input.toString shouldBe label
      input.playerId shouldBe dealerId
    }
  }

  "The player game state filter" should "be available and produce an encodable state" in {
    GameStateFilter[SchnapsenGameState] should not be null
    val codec = PlayerGameState.given_GameStateFilter_SchnapsenGameState.codec
    val state = (Init(round()): SchnapsenGameState).filterForPlayer(dealerId)
    state shouldBe PlayerGameState()
    codec.decodeJson(codec(state)) shouldBe Right(state)
  }

  "The game state codec" should "round-trip every state" in {
    val r = round(
      dealerHand = List(ace(Club)),
      forehandHand = List(king(Spade)),
      talon = List(jack(Club)),
      talonClosing = Some(TalonClosing(forehandId, 3))
    )
    val states = List[SchnapsenGameState](
      Init(r),
      EarlyGameForehandTurn(r, Some(Marriage(Club, Common))),
      EarlyGameDealerTurn(r, ten(Club)),
      LateGameForehandTurn(r),
      LateGameDealerTurn(r, ten(Club)),
      Finish(r, VictoryClaimed(forehandId, dealerId, 3)),
      Finish(r, TalonExhausted(forehandId, dealerId)),
      Exit(r)
    )
    states.foreach(state => decode[SchnapsenGameState](state.asJson.noSpaces) shouldBe Right(state))
  }

  "The input codec" should "round-trip every input" in {
    val inputs = List[SchnapsenInput](
      Start(dealerId),
      Restart(dealerId),
      End(dealerId),
      PlayCard(dealerId, ace(Club)),
      ExchangeTrumpJack(dealerId),
      CloseTalon(dealerId),
      Meld(dealerId, Club),
      ClaimVictory(dealerId)
    )
    inputs.foreach(input => decode[SchnapsenInput](input.asJson.noSpaces) shouldBe Right(input))
  }

  "The player id codecs" should "round-trip a player id as value and as map key" in {
    decode[PlayerId](dealerId.asJson.noSpaces) shouldBe Right(dealerId)
    val scores = Map(dealerId -> 1, forehandId -> 2)
    decode[Map[PlayerId, Int]](scores.asJson.noSpaces) shouldBe Right(scores)
  }
