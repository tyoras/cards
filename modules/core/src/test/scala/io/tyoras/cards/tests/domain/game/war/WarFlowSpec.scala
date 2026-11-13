package io.tyoras.cards.tests.domain.game.war

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.game.GameError.NoPlayersError
import io.tyoras.cards.domain.game.GameTyp
import io.tyoras.cards.domain.game.war.War
import io.tyoras.cards.domain.game.war.model.WarGameState.*
import io.tyoras.cards.domain.game.war.model.WarInput.GameInput.*
import io.tyoras.cards.domain.game.war.model.WarInput.MetaInput.*
import io.tyoras.cards.domain.game.war.model.*
import io.tyoras.cards.tests.{gameIdGen, pick, userIdGen}
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.time.ZonedDateTime

/** Plays the game from crafted states, as the deal of War.apply is random. */
class WarFlowSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val p1: PlayerId = userIdGen.pick
  private val p2: PlayerId = userIdGen.pick
  private val p3: PlayerId = userIdGen.pick
  private val gameId       = gameIdGen.pick

  private val aceS   = Card(Spade, Ace())
  private val kingS  = Card(Spade, King())
  private val kingH  = Card(Heart, King())
  private val tenS   = Card(Spade, Ten())
  private val tenH   = Card(Heart, Ten())
  private val fiveS  = Card(Spade, Five())
  private val fiveH  = Card(Heart, Five())
  private val threeS = Card(Spade, Three())
  private val threeH = Card(Heart, Three())
  private val twoS   = Card(Spade, Two())
  private val twoH   = Card(Heart, Two())
  private val twoC   = Card(Club, Two())

  private def context(hands: (PlayerId, Hand)*): GameContext =
    GameContext(hands.map((id, hand) => id -> Player(id, hand)).toMap, ZonedDateTime.now(), Turn.firstTurn)

  private def battle(hands: (PlayerId, Hand)*): WarGameState = BattleTurn(context(hands*))

  // plays the first card of each listed player, in order
  private def playAll(start: WarGameState, players: PlayerId*): IO[WarGameState] =
    War.fromState[IO](gameId, start).flatMap { war =>
      players.toList.foldLeftM(start) { (_, id) =>
        war.currentState.flatMap(s => war.submitInput(PlayCard(id, s.context.players(id).hand.head.id)))
      }
    }

  private def asWin(state: WarGameState): PlayerWinTurn = state match
    case s: PlayerWinTurn => s
    case other            => fail(s"Expected PlayerWinTurn but got $other")

  private def asWar(state: WarGameState): WarTurn = state match
    case s: WarTurn => s
    case other      => fail(s"Expected WarTurn but got $other")

  // ---- game creation and accessors

  "War.fromState" should "resume the game from the given state" in {
    val state = battle(p1 -> List(aceS), p2 -> List(twoS))
    for
      war     <- War.fromState[IO](gameId, state)
      current <- war.currentState
      ids     <- war.playerIds
    yield
      current shouldBe state
      war.gameId shouldBe gameId
      war.gameType shouldBe GameTyp.War
      ids.toList should contain theSameElementsAs List(p1, p2)
  }

  it should "fail to list the players of a game without any player" in
    War.fromState[IO](gameId, BattleTurn(context())).flatMap(_.playerIds).attempt.asserting(_ shouldBe Left(NoPlayersError))

  "War.apply" should "split the deck between three players" in {
    War[IO](NonEmptyList.of(p1, p2, p3)).flatMap(_.currentState).asserting { state =>
      state.context.players.keySet shouldBe Set(p1, p2, p3)
      state.context.players.values.foreach(_.hand should have size 17)
      state.context.turnNumber shouldBe Turn.firstTurn
    }
  }

  "isFinished" should "be false during the game" in
    War.fromState[IO](gameId, battle(p1 -> List(aceS))).flatMap(_.isFinished).asserting(_ shouldBe false)

  it should "be true on Finish and on Exit" in {
    val ctx = context(p1 -> List(aceS))
    for
      finish <- War.fromState[IO](gameId, Finish(ctx, p1)).flatMap(_.isFinished)
      exit   <- War.fromState[IO](gameId, Exit(ctx)).flatMap(_.isFinished)
    yield
      finish shouldBe true
      exit shouldBe true
  }

  // ---- Init

  "Ready in Init" should "be refused when the player is already ready" in
    War.fromState[IO](gameId, Init(context(p1 -> Nil, p2 -> Nil), Set(p1))).flatMap(_.submitInput(Ready(p1))).attempt.asserting(_ shouldBe Left(WrongPlayer))

  // ---- battle resolution

  "A battle" should "be won by the highest card, the winner collecting all the played cards" in {
    playAll(battle(p1 -> List(kingS, twoS), p2 -> List(fiveS, threeS)), p1, p2).asserting { state =>
      val win = asWin(state)
      win.winnerId shouldBe p1
      win.wonCards shouldBe Set(kingS, fiveS)
      win.eliminated shouldBe empty
      win.context.turnNumber shouldBe Turn(2)
      win.context.players(p1).hand.head shouldBe twoS
      win.context.players(p1).hand should contain theSameElementsAs List(twoS, kingS, fiveS)
      win.context.players(p2).hand shouldBe List(threeS)
    }
  }

  it should "not eliminate a winner who has just played its last card" in {
    playAll(battle(p1 -> List(kingS), p2 -> List(fiveS, threeS)), p1, p2).asserting { state =>
      val win = asWin(state)
      win.eliminated shouldBe empty
      win.context.players(p1).hand should contain theSameElementsAs List(kingS, fiveS)
    }
  }

  it should "eliminate a loser who has no card left" in {
    playAll(battle(p1 -> List(kingS, twoS), p2 -> List(fiveS)), p1, p2).asserting { state =>
      val win = asWin(state)
      win.eliminated shouldBe Set(p2)
      win.context.eliminations shouldBe List(Elimination(p2, Turn.firstTurn))
    }
  }

  it should "only report the newly eliminated players" in {
    val ctx = context(p1 -> List(kingS, twoS), p2 -> List(fiveS), p3 -> Nil).eliminatePlayer(p3)
    playAll(BattleTurn(ctx), p1, p2).asserting { state =>
      val win = asWin(state)
      win.eliminated shouldBe Set(p2)
      win.context.eliminations.map(_.playerId) should contain theSameElementsAs List(p2, p3)
    }
  }

  it should "not expect a card from an eliminated player" in {
    val state = BattleTurn(context(p1 -> List(kingS), p2 -> List(fiveS), p3 -> Nil))
    War.fromState[IO](gameId, state).flatMap(_.submitInput(PlayCard(p3, twoC.id))).attempt.asserting(_ shouldBe Left(WrongPlayer))
  }

  it should "refuse a card which is not the first one of the hand" in {
    War.fromState[IO](gameId, battle(p1 -> List(kingS, twoS), p2 -> List(fiveS))).flatMap(_.submitInput(PlayCard(p1, twoS.id))).attempt.asserting {
      case Left(InvalidCard(_)) => succeed
      case other                => fail(s"Unexpected result $other")
    }
  }

  it should "start a war on a tie" in {
    playAll(battle(p1 -> List(kingS, tenS, fiveS), p2 -> List(kingH, tenH, fiveH)), p1, p2).asserting { state =>
      val war = asWar(state)
      war.battles.size shouldBe 2
      war.currentRound.involvedPlayers.toSortedSet shouldBe Set(p1, p2)
      war.missingHidden shouldBe Set(p1, p2)
      war.missingFighting shouldBe Set(p1, p2)
      war.heap shouldBe Set(kingS, kingH)
    }
  }

  it should "only involve the tied players in the war" in {
    playAll(battle(p1 -> List(kingS, tenS, fiveS), p2 -> List(kingH, tenH, fiveH), p3 -> List(twoC, twoS)), p1, p2, p3).asserting { state =>
      val war = asWar(state)
      war.currentRound.involvedPlayers.toSortedSet shouldBe Set(p1, p2)
      war.missingPlays shouldBe Set(p1, p2)
    }
  }

  // ---- war rounds

  "A war" should "first expect the hidden card of each player, then the fighting one, and be won by the highest fighting card" in {
    val start = battle(p1 -> List(kingS, twoS, tenS, threeS), p2 -> List(kingH, twoH, fiveH))
    for
      war          <- War.fromState[IO](gameId, start)
      _            <- war.submitInput(PlayCard(p1, kingS.id))
      _            <- war.submitInput(PlayCard(p2, kingH.id))
      afterHidden1 <- war.submitInput(PlayCard(p1, twoS.id))
      afterHidden2 <- war.submitInput(PlayCard(p2, twoH.id))
      afterFight1  <- war.submitInput(PlayCard(p1, tenS.id))
      end          <- war.submitInput(PlayCard(p2, fiveH.id))
    yield
      asWar(afterHidden1).missingHidden shouldBe Set(p2)
      asWar(afterHidden2).missingHidden shouldBe empty
      asWar(afterHidden2).missingFighting shouldBe Set(p1, p2)
      asWar(afterFight1).missingFighting shouldBe Set(p2)
      val win = asWin(end)
      win.winnerId shouldBe p1
      win.wonCards shouldBe Set(kingS, kingH, twoS, twoH, tenS, fiveH)
      win.context.players(p1).hand should contain theSameElementsAs List(threeS, kingS, kingH, twoS, twoH, tenS, fiveH)
      win.eliminated shouldBe Set(p2)
  }

  it should "refuse a card from a player who has already played" in {
    for
      war    <- War.fromState[IO](gameId, battle(p1 -> List(kingS, twoS, tenS), p2 -> List(kingH, twoH, tenH)))
      _      <- war.submitInput(PlayCard(p1, kingS.id))
      _      <- war.submitInput(PlayCard(p2, kingH.id))
      _      <- war.submitInput(PlayCard(p1, twoS.id))
      _      <- war.submitInput(PlayCard(p1, tenS.id))
      result <- war.submitInput(PlayCard(p1, tenS.id)).attempt
    yield result shouldBe Left(WrongPlayer)
  }

  it should "refuse a card which is not the first one of the hand" in {
    for
      war    <- War.fromState[IO](gameId, battle(p1 -> List(kingS, twoS, tenS), p2 -> List(kingH, twoH)))
      _      <- war.submitInput(PlayCard(p1, kingS.id))
      _      <- war.submitInput(PlayCard(p2, kingH.id))
      result <- war.submitInput(PlayCard(p1, tenS.id)).attempt
    yield result.isLeft shouldBe true
  }

  it should "start a new round on a tie between the fighting cards" in {
    val start = battle(p1 -> List(kingS, twoS, tenS, threeS), p2 -> List(kingH, twoH, tenH, threeH))
    playAll(start, p1, p2, p1, p2, p1, p2).asserting { state =>
      val war = asWar(state)
      war.battles.size shouldBe 3
      war.missingPlays shouldBe Set(p1, p2)
      war.heap shouldBe Set(kingS, kingH, twoS, twoH, tenS, tenH)
    }
  }

  it should "use the hidden cards when no player has a fighting card left" in {
    playAll(battle(p1 -> List(kingS, tenS), p2 -> List(kingH, twoH)), p1, p2, p1, p2).asserting { state =>
      val win = asWin(state)
      win.winnerId shouldBe p1
      win.wonCards shouldBe Set(kingS, kingH, tenS, twoH)
      win.eliminated shouldBe Set(p2)
    }
  }

  it should "draw the deciding cards from another deck when no player has a card left" in {
    playAll(battle(p1 -> List(kingS), p2 -> List(kingH)), p1, p2).asserting { state =>
      val win = asWin(state)
      Set(p1, p2) should contain(win.winnerId)
      win.wonCards shouldBe Set(kingS, kingH)
      win.eliminated shouldBe Set(p1, p2) - win.winnerId
    }
  }

  // ---- turn acknowledgement and end of the game

  private def winTurn(ctx: GameContext, acked: Set[PlayerId] = Set.empty): WarGameState = PlayerWinTurn(ctx, p1, Set(aceS), Set.empty, acked)

  "Ready after a won turn" should "wait for all the players" in {
    War.fromState[IO](gameId, winTurn(context(p1 -> List(aceS), p2 -> List(twoS)))).flatMap(_.submitInput(Ready(p2))).asserting { state =>
      asWin(state).acked shouldBe Set(p2)
      asWin(state).notAcked shouldBe Set(p1)
    }
  }

  it should "be refused from a player who has already acknowledged" in {
    War
      .fromState[IO](gameId, winTurn(context(p1 -> List(aceS), p2 -> List(twoS)), Set(p2)))
      .flatMap(_.submitInput(Ready(p2)))
      .attempt
      .asserting(_ shouldBe Left(WrongPlayer))
  }

  it should "start the next battle when the game goes on" in {
    val ctx = context(p1 -> List(aceS), p2 -> List(twoS))
    War.fromState[IO](gameId, winTurn(ctx, Set(p2))).flatMap(_.submitInput(Ready(p1))).asserting(_ shouldBe BattleTurn(ctx))
  }

  it should "finish the game with the last player standing" in {
    val ctx = context(p1 -> List(aceS), p2 -> Nil).eliminatePlayer(p2)
    for
      war      <- War.fromState[IO](gameId, winTurn(ctx, Set(p2)))
      state    <- war.submitInput(Ready(p1))
      finished <- war.isFinished
    yield
      state shouldBe Finish(ctx, p1)
      finished shouldBe true
      state.asInstanceOf[Finish].losers shouldBe List(p2)
      state.label shouldBe "Finish"
      state.code shouldBe "finish"
  }

  it should "fail when the game is over without a unique winner" in {
    val ctx = context(p1 -> List(aceS), p2 -> List(twoS)).eliminatePlayer(p2)
    War.fromState[IO](gameId, winTurn(ctx, Set(p2))).flatMap(_.submitInput(Ready(p1))).attempt.asserting {
      case Left(InvalidState(_)) => succeed
      case other                 => fail(s"Unexpected result $other")
    }
  }

  // ---- meta inputs in the middle of the game

  private def metaInput(input: WarInput)(check: WarGameState => Assertion): IO[Assertion] =
    War.fromState[IO](gameId, battle(p1 -> List(aceS), p2 -> List(twoS))).flatMap(_.submitInput(input)).asserting(check)

  "Restart" should "deal a new game for the same players from any state" in {
    metaInput(Restart(p1)) {
      case s: Init =>
        s.context.players.keySet shouldBe Set(p1, p2)
        s.context.players.values.foreach(_.hand should have size 26)
        s.ready shouldBe empty
      case other => fail(s"Unexpected state $other")
    }
  }

  "End" should "exit the game from any state" in {
    metaInput(End(p1)) { state =>
      state shouldBe a[Exit]
      state.code shouldBe "exit"
    }
  }

  "A Ready input during a battle" should "be ignored" in {
    val state = battle(p1 -> List(aceS), p2 -> List(twoS))
    War.fromState[IO](gameId, state).flatMap(_.submitInput(Ready(p1))).asserting(_ shouldBe state)
  }
