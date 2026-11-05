package io.tyoras.cards.tests.domain.game

import cats.data.NonEmptyList
import cats.effect.{IO, Ref}
import cats.effect.testing.scalatest.AsyncIOSpec
import fs2.Stream
import io.circe.{Decoder, Encoder}
import io.tyoras.cards.domain.game.GameError.{GameAlreadyFinished, GameNotFound}
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.domain.game.war.model.{GameContext, GameState, Turn}
import io.tyoras.cards.domain.game.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.tests.{gameIdGen, pick, userIdGen}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{ZoneOffset, ZonedDateTime}

class GameServiceSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val now     = ZonedDateTime.of(2026, 1, 1, 12, 0, 0, 0, ZoneOffset.UTC)
  private val later   = now.plusHours(1)
  private val user1   = userIdGen.pick
  private val user2   = userIdGen.pick
  private val context = GameContext(Map.empty, now, Turn.firstTurn)
  private val state   = GameState.Init(context): GameState
  private val final_  = GameState.Exit(context): GameState

  private def data(finishedAt: Option[ZonedDateTime] = None, players: NonEmptyList[User.ID] = NonEmptyList.of(user1, user2)): Game.Data[GameState] =
    Game.Data[GameState](GameTyp.War, players, state, user1, finishedAt)

  private def existing(finishedAt: Option[ZonedDateTime] = None, players: NonEmptyList[User.ID] = NonEmptyList.of(user1, user2)): Game.Existing[GameState] =
    Game.Existing(gameIdGen.pick, now, now, data(finishedAt, players))

  private class InMemoryRepo(games: Ref[IO, List[Game.Existing[?]]]) extends GameRepository[IO]:
    override def insert[State : Decoder : Encoder](data: Game.Data[State], withId: Option[Game.ID]): IO[Game.Existing[State]] =
      val game = Game.Existing(withId.getOrElse(gameIdGen.pick), now, now, data)
      games.update(_ :+ game).as(game)

    override def update[State : Decoder : Encoder](game: Game.Existing[State]): IO[Game.Existing[State]] =
      games.update(_.map(g => if g.id == game.id then game else g)).as(game)

    override def readAll[State : Decoder](finished: Boolean): Stream[IO, Game.Existing[State]] =
      Stream.evalSeq(games.get.map(_.filter(_.data.finishedAt.isDefined == finished).map(_.asInstanceOf[Game.Existing[State]])))

    override def readManyById[State : Decoder](ids: List[Game.ID]): IO[List[Game.Existing[State]]] =
      games.get.map(_.filter(g => ids.contains(g.id)).map(_.asInstanceOf[Game.Existing[State]]))

    override def readManyByUser[State : Decoder](userId: User.ID, finished: Boolean): IO[List[Game.Existing[State]]] =
      games.get.map(_.filter(g => g.players.toList.contains(userId) && g.data.finishedAt.isDefined == finished).map(_.asInstanceOf[Game.Existing[State]]))

    override def deleteMany(toDelete: List[Game.Existing[?]]): IO[Unit] =
      games.update(_.filterNot(g => toDelete.exists(_.id == g.id)))

    override def deleteAll: IO[Unit] = games.set(Nil)

  private case class Fixture(service: GameService[IO], repo: GameRepository[IO], games: Ref[IO, List[Game.Existing[?]]])

  private def fixture(initial: Game.Existing[?]*): IO[Fixture] =
    Ref.of[IO, List[Game.Existing[?]]](initial.toList).map { games =>
      val repo = new InMemoryRepo(games)
      Fixture(GameService.of[IO](repo), repo, games)
    }

  "create" should "persist a new game with a generated id by default" in {
    for
      f       <- fixture()
      created <- f.service.create(data())
      stored  <- f.games.get
    yield
      created.data shouldBe data()
      stored shouldBe List(created)
  }

  it should "use the provided id" in {
    val id = gameIdGen.pick
    fixture().flatMap(_.service.create(data(), Some(id))).asserting(_.id shouldBe id)
  }

  "GameRepository.insert" should "not force an id by default" in
    fixture().flatMap(_.repo.insert(data())).asserting(_.data shouldBe data())

  "update" should "persist the game" in {
    val game    = existing()
    val updated = game.withUpdatedState(final_)
    for
      f      <- fixture(game)
      result <- f.service.update(updated)
      stored <- f.games.get
    yield
      result shouldBe updated
      stored shouldBe List(updated)
  }

  "finish" should "save the final state and the end date" in {
    val game = existing()
    for
      f      <- fixture(game)
      result <- f.service.finish(game.id, final_, later)
      stored <- f.games.get
    yield
      result.state shouldBe final_
      result.data.finishedAt shouldBe Some(later)
      stored shouldBe List(result)
  }

  it should "fail when the game does not exist" in {
    val id = gameIdGen.pick
    fixture().flatMap(_.service.finish(id, final_, later)).attempt.asserting(_ shouldBe Left(GameNotFound(id)))
  }

  it should "fail and change nothing when the game is already finished" in {
    val game = existing(finishedAt = Some(now))
    for
      f      <- fixture(game)
      result <- f.service.finish(game.id, final_, later).attempt
      stored <- f.games.get
    yield
      result shouldBe Left(GameAlreadyFinished(game.id))
      stored shouldBe List(game)
  }

  "readById" should "return the game when it exists" in {
    val game = existing()
    fixture(game).flatMap(_.service.readById[GameState](game.id)).asserting(_ shouldBe Some(game))
  }

  it should "return nothing when the game does not exist" in
    fixture().flatMap(_.service.readById[GameState](gameIdGen.pick)).asserting(_ shouldBe None)

  "readManyByUser" should "return the games of the user with the requested status" in {
    val ongoing  = existing()
    val finished = existing(finishedAt = Some(now))
    val other    = existing(players = NonEmptyList.one(user2))
    for
      f       <- fixture(ongoing, finished, other)
      current <- f.service.readManyByUser[GameState](user1, finished = false)
      past    <- f.service.readManyByUser[GameState](user1, finished = true)
    yield
      current shouldBe List(ongoing)
      past shouldBe List(finished)
  }

  "readAll" should "stream the games with the requested status" in {
    val ongoing  = existing()
    val finished = existing(finishedAt = Some(now))
    fixture(ongoing, finished).flatMap(_.service.readAll[GameState](finished = true).compile.toList).asserting(_ shouldBe List(finished))
  }

  "delete" should "remove only the given game" in {
    val game  = existing()
    val other = existing()
    for
      f      <- fixture(game, other)
      _      <- f.service.delete(game)
      stored <- f.games.get
    yield stored shouldBe List(other)
  }

  "deleteMany" should "remove the given games" in {
    val game1 = existing()
    val game2 = existing()
    val other = existing()
    for
      f      <- fixture(game1, game2, other)
      _      <- f.service.deleteMany(List(game1, game2))
      stored <- f.games.get
    yield stored shouldBe List(other)
  }

  "deleteAll" should "remove all the games" in {
    for
      f      <- fixture(existing(), existing())
      _      <- f.service.deleteAll
      stored <- f.games.get
    yield stored shouldBe empty
  }
