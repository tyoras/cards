package io.tyoras.cards.tests.domain.game.ers

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import io.tyoras.cards.domain.card.{Card, Hand, Turn}
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.Suit.*
import io.tyoras.cards.domain.game.GameTyp
import io.tyoras.cards.domain.game.ers.EgyptianRatscrew
import io.tyoras.cards.domain.game.ers.model.*
import io.tyoras.cards.domain.game.ers.model.ERsGameState.*
import io.tyoras.cards.domain.game.ers.model.ERSInput.{GameInput, MetaInput}
import io.tyoras.cards.domain.game.ers.model.ERSInput.GameInput.{PlayCard, Ready, Slap}
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.tests.{gameIdGen, pick, userIdGen}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.time.ZonedDateTime

class EgyptianRatscrewSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val playerIds   = NonEmptyList.of(userIdGen.pick, userIdGen.pick)

  private val p1: PlayerId = userIdGen.pick
  private val p2: PlayerId = userIdGen.pick
  private val p3: PlayerId = userIdGen.pick
  private val gameId       = gameIdGen.pick

  private def ctx(hands: (PlayerId, Hand)*): GameContext =
    GameContext(
      hands.map((id, hand) => id -> Player(id, hand)).toMap,
      ZonedDateTime.now(),
      Turn.firstTurn,
      NonEmptyList.fromListUnsafe(hands.map(_._1).toList)
    )

  private def game(state: ERsGameState): IO[EgyptianRatscrew[IO]] = EgyptianRatscrew.fromState[IO](gameId, state)

  private def play(state: ERsGameState, inputs: ERSInput*): IO[ERsGameState] =
    game(state).flatMap(g => inputs.foldLeft(g.currentState)((acc, i) => acc >> g.submitInput(i)))

  private def play(playerId: PlayerId, card: Card): PlayCard = PlayCard(playerId, card.id)

  // cards
  private val two   = Card(Spade, Two())
  private val three = Card(Spade, Three())
  private val four  = Card(Spade, Four())
  private val five  = Card(Spade, Five())
  private val six   = Card(Heart, Six())
  private val seven = Card(Heart, Seven())
  private val jack  = Card(Club, Jack())
  private val queen = Card(Club, Queen())
  private val ace   = Card(Diamond, Ace())

  // ---------------------------------------------------------------------------------------------------------------- //
  // creation

  "EgyptianRatscrew.apply" should "initialize game with Init state" in {
    EgyptianRatscrew[IO](playerIds).flatMap(_.currentState).asserting {
      case _: Init => succeed
      case state   => fail(s"Expected Init state but got $state")
    }
  }

  it should "distribute cards evenly among players" in {
    EgyptianRatscrew[IO](playerIds).flatMap(_.currentState).asserting {
      case init: Init =>
        val handSizes = init.context.players.values.map(_.hand.size).toList
        handSizes shouldBe List(26, 26)
      case state => fail(s"Expected Init state but got $state")
    }
  }

  it should "use the player ids as the player order and start at the first turn" in {
    EgyptianRatscrew[IO](playerIds).flatMap(_.currentState).asserting { s =>
      s.context.playerOrder shouldBe playerIds
      s.context.turnNumber shouldBe Turn.firstTurn
      s.context.eliminations shouldBe empty
    }
  }

  it should "expose the game type, the player ids and not be finished" in {
    for
      g        <- EgyptianRatscrew[IO](playerIds)
      ids      <- g.playerIds
      finished <- g.isFinished
    yield
      g.gameType shouldBe GameTyp.EgyptianRatscrew
      ids.toList should contain theSameElementsAs playerIds.toList
      finished shouldBe false
  }

  "EgyptianRatscrew.fromState" should "resume the game from the given state" in {
    val state = PlayerTurn(ctx(p1 -> List(two), p2 -> List(three)), p2, List(four))
    game(state).flatMap(g => g.currentState.map(g.gameId -> _)).asserting { (id, s) =>
      id shouldBe gameId
      s shouldBe state
    }
  }

  "EgyptianRatscrew.isFinished" should "be true in Finish state" in
    game(Finish(ctx(p1 -> List(two), p2 -> Nil), p1)).flatMap(_.isFinished).asserting(_ shouldBe true)

  it should "be true in Exit state" in
    game(Exit(ctx(p1 -> List(two), p2 -> Nil))).flatMap(_.isFinished).asserting(_ shouldBe true)

  // ---------------------------------------------------------------------------------------------------------------- //
  // Init

  "Init state" should "stay in Init when not all players are ready" in {
    val init = Init(ctx(p1 -> List(two), p2 -> List(three)))
    play(init, Ready(p1)).asserting(_ shouldBe init.copy(ready = Set(p1)))
  }

  it should "transition to PlayerTurn for the first player of the order when all players are ready" in {
    val context = ctx(p2 -> List(three), p1 -> List(two))
    play(Init(context), Ready(p1), Ready(p2)).asserting(_ shouldBe PlayerTurn(context, p2))
  }

  it should "reject a player that is already ready" in {
    play(Init(ctx(p1 -> List(two), p2 -> List(three))), Ready(p1), Ready(p1)).assertThrows[WrongPlayer.type]
  }

  it should "reject an unknown player" in {
    play(Init(ctx(p1 -> List(two), p2 -> List(three))), Ready(p3)).assertThrows[WrongPlayer.type]
  }

  // ---------------------------------------------------------------------------------------------------------------- //
  // PlayerTurn

  "PlayerTurn state" should "give the turn to the next player when a number card is played" in {
    val context = ctx(p1 -> List(two, three), p2 -> List(four))
    play(PlayerTurn(context, p1, List(five)), play(p1, two)).asserting {
      case PlayerTurn(c, player, pile) =>
        player shouldBe p2
        pile shouldBe List(two, five)
        c.playerHand(p1) shouldBe List(three)
      case s => fail(s"Expected PlayerTurn but got $s")
    }
  }

  it should "start a challenge when a head card is played" in {
    val context = ctx(p1 -> List(queen, three), p2 -> List(four))
    play(PlayerTurn(context, p1, List(five)), play(p1, queen)).asserting {
      case ChallengeTurn(c, owner, challenger, total, remaining, pile) =>
        owner shouldBe p1
        challenger shouldBe p2
        total shouldBe 2
        remaining shouldBe 2
        pile shouldBe NonEmptyList.of(queen, five)
        c.playerHand(p1) shouldBe List(three)
      case s => fail(s"Expected ChallengeTurn but got $s")
    }
  }

  it should "let the current player win the pile when no other player has cards" in {
    val context = ctx(p1 -> List(two, three), p2 -> Nil)
    play(PlayerTurn(context, p1, List(five)), play(p1, two)).asserting {
      case PlayerWinTurn(c, winner, wonCards, eliminated, acked) =>
        winner shouldBe p1
        wonCards shouldBe NonEmptyList.of(two, five)
        eliminated shouldBe Set(p2)
        acked shouldBe empty
        c.playerHand(p1) shouldBe List(three, five, two)
        c.isEliminated(p2) shouldBe true
      case s => fail(s"Expected PlayerWinTurn but got $s")
    }
  }

  it should "reject a card played by another player" in {
    play(PlayerTurn(ctx(p1 -> List(two), p2 -> List(three)), p1), play(p2, three)).assertThrows[WrongPlayer.type]
  }

  it should "reject a card that is not the first card of the player hand" in {
    play(PlayerTurn(ctx(p1 -> List(two, three), p2 -> List(four)), p1), play(p1, three)).assertThrows[InvalidCard]
  }

  it should "reject a slap on an empty pile" in {
    play(PlayerTurn(ctx(p1 -> List(two), p2 -> List(three)), p1), Slap(p2)).assertThrows[InvalidAction]
  }

  it should "let the slapper win the pile on a valid slap" in {
    val pile    = List(Card(Heart, Five()), five, two)
    val context = ctx(p1 -> List(three), p2 -> List(four))
    play(PlayerTurn(context, p1, pile), Slap(p2)).asserting {
      case PlayerWinTurn(c, winner, wonCards, eliminated, _) =>
        winner shouldBe p2
        wonCards.toList shouldBe pile
        eliminated shouldBe empty
        c.playerHand(p2) shouldBe four :: pile.reverse
      case s => fail(s"Expected PlayerWinTurn but got $s")
    }
  }

  it should "penalize the slapper on an invalid slap" in {
    val state = PlayerTurn(ctx(p1 -> List(three), p2 -> List(four)), p1, List(five, two))
    play(state, Slap(p2)).asserting(_ shouldBe SlapPenalty(state.context, p2, NonEmptyList.of(five, two), state))
  }

  it should "penalize the slapper when the pile contains a single card" in {
    val state = PlayerTurn(ctx(p1 -> List(three), p2 -> List(four)), p1, List(five))
    play(state, Slap(p1)).asserting(_ shouldBe a[SlapPenalty])
  }

  it should "ignore a Ready input" in {
    val state = PlayerTurn(ctx(p1 -> List(two), p2 -> List(three)), p1)
    play(state, Ready(p1)).asserting(_ shouldBe state)
  }

  it should "follow the player order with more than 2 players" in {
    val context = ctx(p1 -> List(two), p2 -> List(three), p3 -> List(four))
    play(PlayerTurn(context, p2), play(p2, three)).asserting {
      case s: PlayerTurn => s.player shouldBe p3
      case s             => fail(s"Expected PlayerTurn but got $s")
    }
  }

  it should "follow the player order with more than 2 players but ignore players with no cards" in {
    val context = ctx(p1 -> List(two), p2 -> List(three), p3 -> List.empty)
    play(PlayerTurn(context, p2), play(p2, three)).asserting {
      case s: PlayerTurn => s.player shouldBe p1
      case s             => fail(s"Expected PlayerTurn but got $s")
    }
  }

  // ---------------------------------------------------------------------------------------------------------------- //
  // ChallengeTurn

  private def challenge(p1Hand: Hand, p2Hand: Hand, remaining: Int = 2) =
    ChallengeTurn(ctx(p1 -> p1Hand, p2 -> p2Hand), owner = p1, challenger = p2, totalTries = 2, remainingTries = remaining, pile = NonEmptyList.of(queen))

  "ChallengeTurn state" should "decrement the remaining tries when the challenger plays a number card" in {
    play(challenge(List(two), List(three, four)), play(p2, three)).asserting {
      case ChallengeTurn(c, owner, challenger, total, remaining, pile) =>
        owner shouldBe p1
        challenger shouldBe p2
        total shouldBe 2
        remaining shouldBe 1
        pile shouldBe NonEmptyList.of(three, queen)
        c.playerHand(p2) shouldBe List(four)
      case s => fail(s"Expected ChallengeTurn but got $s")
    }
  }

  it should "let the owner win the pile when the challenger has no try left" in {
    play(challenge(List(two), List(three, four), remaining = 1), play(p2, three)).asserting {
      case PlayerWinTurn(c, winner, wonCards, eliminated, _) =>
        winner shouldBe p1
        wonCards shouldBe NonEmptyList.of(three, queen)
        eliminated shouldBe empty
        c.playerHand(p1) shouldBe List(two, queen, three)
      case s => fail(s"Expected PlayerWinTurn but got $s")
    }
  }

  it should "reverse the challenge when the challenger plays a head card" in {
    play(challenge(List(two), List(ace, four)), play(p2, ace)).asserting {
      case ChallengeTurn(_, owner, challenger, total, remaining, pile) =>
        owner shouldBe p2
        challenger shouldBe p1
        total shouldBe 4
        remaining shouldBe 4
        pile shouldBe NonEmptyList.of(ace, queen)
      case s => fail(s"Expected ChallengeTurn but got $s")
    }
  }

  it should "give the challenge to the next player when the challenger runs out of cards" in {
    play(challenge(List(two), List(three)), play(p2, three)).asserting {
      case ChallengeTurn(_, owner, challenger, _, remaining, _) =>
        owner shouldBe p1
        challenger shouldBe p1
        remaining shouldBe 1
      case s => fail(s"Expected ChallengeTurn but got $s")
    }
  }

  it should "let the owner win the pile when no player has cards left" in {
    play(challenge(Nil, List(three)), play(p2, three)).asserting {
      case PlayerWinTurn(c, winner, wonCards, _, _) =>
        winner shouldBe p1
        wonCards shouldBe NonEmptyList.of(three, queen)
        c.playerHand(p1) shouldBe List(queen, three)
      case s => fail(s"Expected PlayerWinTurn but got $s")
    }
  }

  it should "not eliminate the owner winning the pile with an empty hand" in {
    play(challenge(Nil, List(three)), play(p2, three)).asserting {
      case s: PlayerWinTurn =>
        s.eliminated shouldBe Set(p2)
        s.context.isEliminated(p1) shouldBe false
        s.context.isEliminated(p2) shouldBe true
      case s => fail(s"Expected PlayerWinTurn but got $s")
    }
  }

  it should "reject a card played by another player than the challenger" in {
    play(challenge(List(two), List(three)), play(p1, two)).assertThrows[WrongPlayer.type]
  }

  it should "reject an invalid card" in {
    play(challenge(List(two), List(three, four)), play(p2, four)).assertThrows[InvalidCard]
  }

  it should "let the slapper win the pile on a valid slap" in {
    val state = challenge(List(two), List(three)).copy(pile = NonEmptyList.of(jack, Card(Heart, Jack()), queen))
    play(state, Slap(p2)).asserting {
      case PlayerWinTurn(_, winner, wonCards, _, _) =>
        winner shouldBe p2
        wonCards shouldBe state.pile
      case s => fail(s"Expected PlayerWinTurn but got $s")
    }
  }

  it should "penalize the slapper on an invalid slap" in {
    val state = challenge(List(two), List(three))
    play(state, Slap(p1)).asserting(_ shouldBe SlapPenalty(state.context, p1, state.pile, state))
  }

  // ---------------------------------------------------------------------------------------------------------------- //
  // SlapPenalty

  private val turnBeforeSlap = PlayerTurn(ctx(p1 -> List(two, three), p2 -> List(four), p3 -> List(six)), p2, List(five, seven))
  private val penaltyState   = SlapPenalty(turnBeforeSlap.context, p1, NonEmptyList.of(five, seven), turnBeforeSlap)

  "SlapPenalty state" should "record the acks of the other players" in {
    play(penaltyState, Ready(p2)).asserting {
      case s: SlapPenalty =>
        s.acked shouldBe Set(p2)
        s.notAcked shouldBe Set(p3)
      case s => fail(s"Expected SlapPenalty but got $s")
    }
  }

  it should "reject an ack from the penalized player" in {
    play(penaltyState, Ready(p1)).assertThrows[WrongPlayer.type]
  }

  it should "reject an ack from a player that has already acked" in {
    play(penaltyState, Ready(p2), Ready(p2)).assertThrows[WrongPlayer.type]
  }

  it should "let the penalized player discard its first card" in {
    play(penaltyState, play(p1, two)).asserting {
      case s: SlapPenalty =>
        s.discardedCards shouldBe List(two)
        s.context.playerHand(p1) shouldBe List(three)
        s.penaltyPayed shouldBe true
      case s => fail(s"Expected SlapPenalty but got $s")
    }
  }

  it should "reject a discard from another player" in {
    play(penaltyState, play(p2, four)).assertThrows[WrongPlayer.type]
  }

  it should "reject an invalid discarded card" in {
    play(penaltyState, play(p1, three)).assertThrows[InvalidCard]
  }

  it should "reject a discard once the penalty is payed" in {
    play(penaltyState, play(p1, two), play(p1, three)).assertThrows[InvalidAction]
  }

  it should "resume the PlayerTurn with the discarded cards under the pile once payed and acked" in {
    play(penaltyState, Ready(p2), play(p1, two), Ready(p3)).asserting {
      _ shouldBe turnBeforeSlap.copy(
        context = turnBeforeSlap.context.updatePlayer(p1)(_.copy(hand = List(three))),
        pile = List(five, seven, two)
      )
    }
  }

  it should "resume the ChallengeTurn with the discarded cards under the pile once payed and acked" in {
    val original = challenge(List(two, three), List(four))
    val penalty  = SlapPenalty(original.context, p1, original.pile, original)
    play(penalty, play(p1, two), Ready(p2)).asserting {
      _ shouldBe original.copy(
        context = original.context.updatePlayer(p1)(_.copy(hand = List(three))),
        pile = NonEmptyList.of(queen, two)
      )
    }
  }

  it should "consider the penalty payed when the penalized player has no cards" in {
    val original = PlayerTurn(ctx(p1 -> Nil, p2 -> List(four)), p2, List(five))
    play(SlapPenalty(original.context, p1, NonEmptyList.of(five), original), Ready(p2)).asserting(_ shouldBe original)
  }

  // ---------------------------------------------------------------------------------------------------------------- //
  // PlayerWinTurn

  private val winTurn = PlayerWinTurn(ctx(p1 -> List(two, three), p2 -> List(four)), p2, NonEmptyList.of(four), Set.empty)

  "PlayerWinTurn state" should "record the acks of the players" in
    play(winTurn, Ready(p1)).asserting(_ shouldBe winTurn.copy(acked = Set(p1)))

  it should "start the next turn with the winner once all players acked" in
    play(winTurn, Ready(p1), Ready(p2)).asserting(_ shouldBe PlayerTurn(winTurn.context.incrementTurnNumber, p2))

  it should "reject an ack from a player that has already acked" in {
    play(winTurn, Ready(p1), Ready(p1)).assertThrows[WrongPlayer.type]
  }

  it should "finish the game when all players but one are eliminated" in {
    val context = ctx(p1 -> List(two, three), p2 -> Nil).eliminatePlayer(p2)
    val state   = PlayerWinTurn(context, p1, NonEmptyList.of(two), Set(p2))
    play(state, Ready(p1), Ready(p2)).asserting(_ shouldBe Finish(context, p1))
  }

  it should "raise an error when the game is finished without a unique winner" in {
    val context = ctx(p1 -> Nil, p2 -> Nil).eliminatePlayer(p2)
    play(PlayerWinTurn(context, p1, NonEmptyList.of(two), Set(p2)), Ready(p1), Ready(p2)).assertThrows[InvalidState]
  }

  // ---------------------------------------------------------------------------------------------------------------- //
  // meta inputs

  "EgyptianRatscrew.submitInput" should "restart game when Restart input received" in {
    for {
      war  <- EgyptianRatscrew[IO](playerIds)
      init <- war.currentState
      playerId = init.context.players.keys.head
      state <- war.submitInput(MetaInput.Restart(playerId))
    } yield {
      state shouldBe a[Init]
      state.asInstanceOf[Init].ready shouldBe empty
    }
  }

  it should "restart from any state with freshly dealt hands" in {
    play(winTurn, MetaInput.Restart(p1)).asserting {
      case Init(c, ready) =>
        ready shouldBe empty
        c.players.keySet shouldBe Set(p1, p2)
        c.players.values.map(_.hand.size).toList shouldBe List(26, 26)
        c.turnNumber shouldBe Turn.firstTurn
      case s => fail(s"Expected Init but got $s")
    }
  }

  it should "transition to Exit when End input received" in {
    for {
      war  <- EgyptianRatscrew[IO](playerIds)
      init <- war.currentState
      playerId = init.context.players.keys.head
      state <- war.submitInput(MetaInput.End(playerId))
    } yield state shouldBe a[Exit]
  }

  it should "exit from any state keeping the context" in
    play(penaltyState, MetaInput.End(p2)).asserting(_ shouldBe Exit(penaltyState.context))

  it should "ignore wrong input in any state" in {
    for {
      war  <- EgyptianRatscrew[IO](playerIds)
      init <- war.currentState
      playerId    = init.context.players.keys.head
      invalidCard = Card(Heart, King())
      _     <- war.submitInput(GameInput.PlayCard(playerId, invalidCard.id))
      state <- war.currentState
    } yield state shouldBe init
  }

  it should "ignore game inputs in Finish and Exit states" in {
    val finish = Finish(ctx(p1 -> List(two), p2 -> Nil), p1)
    val exit   = Exit(finish.context)
    for
      s1 <- play(finish, Ready(p1), Slap(p2), play(p1, two))
      s2 <- play(exit, Ready(p1), Slap(p2), play(p1, two))
    yield
      s1 shouldBe finish
      s2 shouldBe exit
  }
}
