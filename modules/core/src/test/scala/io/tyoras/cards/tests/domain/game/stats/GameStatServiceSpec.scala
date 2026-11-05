package io.tyoras.cards.tests.domain.game.stats

import cats.data.NonEmptyList
import cats.effect.{IO, Ref}
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.show.*
import fs2.Stream
import io.github.iltotore.iron.*
import io.tyoras.cards.domain.game.stats.model.{PlayerGameStat, PlayerGameStats}
import io.tyoras.cards.domain.game.stats.{GameStatRepository, GameStatService}
import io.tyoras.cards.domain.game.{GameTyp, GameType}
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.tests.{pick, userIdGen}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{ZoneOffset, ZonedDateTime}
import java.util.UUID

class GameStatServiceSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val now    = ZonedDateTime.of(2026, 1, 1, 12, 0, 0, 0, ZoneOffset.UTC)
  private val winner = userIdGen.pick
  private val drawer = userIdGen.pick
  private val loser  = userIdGen.pick
  private val game   = GameTyp.War

  private def existing(playerId: User.ID, gameType: GameType = game): PlayerGameStat.Existing =
    PlayerGameStat.Existing(PlayerGameStat.ID(UUID.randomUUID()), now, now, PlayerGameStat.Data(playerId, gameType, 1, 2, 3))

  private class InMemoryRepo(stats: Ref[IO, List[PlayerGameStat.Existing]]) extends GameStatRepository[IO]:
    override def writeMany(toWrite: NonEmptyList[PlayerGameStat]): IO[NonEmptyList[PlayerGameStat.Existing]] =
      val written = toWrite.map {
        case e: PlayerGameStat.Existing => e
        case d: PlayerGameStat.Data     => PlayerGameStat.Existing(PlayerGameStat.ID(UUID.randomUUID()), now, now, d)
      }
      stats.update(current => written.toList ++ current.filterNot(c => written.exists(_.id == c.id))).as(written)

    override def insert(data: PlayerGameStat.Data, withId: Option[PlayerGameStat.ID]): IO[PlayerGameStat.Existing] =
      val stat = PlayerGameStat.Existing(withId.getOrElse(PlayerGameStat.ID(UUID.randomUUID())), now, now, data)
      stats.update(_ :+ stat).as(stat)

    override def readManyById(ids: NonEmptyList[PlayerGameStat.ID]): IO[List[PlayerGameStat.Existing]] =
      stats.get.map(_.filter(s => ids.exists(_ == s.id)))

    override def readManyByPlayer(playerId: User.ID): IO[List[PlayerGameStat.Existing]] =
      stats.get.map(_.filter(_.playerId == playerId))

    override def readOne(playerId: User.ID, gameType: GameType): IO[Option[PlayerGameStat.Existing]] =
      stats.get.map(_.find(s => s.playerId == playerId && s.game == gameType))

    override def readAll: Stream[IO, PlayerGameStat.Existing] = Stream.evalSeq(stats.get)

    override def deleteMany(toDelete: NonEmptyList[PlayerGameStat.Existing]): IO[Unit] =
      stats.update(_.filterNot(s => toDelete.exists(_.id == s.id)))

    override def deleteAll: IO[Unit] = stats.set(Nil)

  private case class Fixture(service: GameStatService[IO], repo: GameStatRepository[IO], stats: Ref[IO, List[PlayerGameStat.Existing]])

  private def fixture(initial: PlayerGameStat.Existing*): IO[Fixture] =
    Ref.of[IO, List[PlayerGameStat.Existing]](initial.toList).map { stats =>
      val repo = new InMemoryRepo(stats)
      Fixture(GameStatService.of[IO](repo), repo, stats)
    }

  "updatePlayersStats" should "create the stats of players without any stats" in {
    fixture().flatMap(_.service.updatePlayersStats(game, List(winner), List(drawer), List(loser))).asserting { saved =>
      val byPlayer = saved.toList.map(s => s.playerId -> (s.won: Int, s.draw: Int, s.lost: Int)).toMap
      byPlayer shouldBe Map(winner -> (1, 0, 0), drawer -> (0, 1, 0), loser -> (0, 0, 1))
      saved.toList.map(_.game).toSet shouldBe Set(game)
    }
  }

  it should "increment the existing stats of the players" in {
    val stat = existing(winner)
    fixture(stat).flatMap(_.service.updatePlayersStats(game, List(winner), Nil, Nil)).asserting { saved =>
      val updated = saved.head
      updated.id shouldBe stat.id
      (updated.won: Int, updated.draw: Int, updated.lost: Int) shouldBe (2, 2, 3)
    }
  }

  it should "not mix the stats of another game" in {
    val otherGame = existing(loser, GameTyp.Schnapsen)
    fixture(otherGame).flatMap(_.service.updatePlayersStats(game, Nil, Nil, List(loser))).asserting { saved =>
      saved.head.id should not be otherGame.id
      saved.head.lost shouldBe 1
    }
  }

  it should "fail when there is no player to update" in
    fixture().flatMap(_.service.updatePlayersStats(game, Nil, Nil, Nil)).attempt.asserting(_.left.toOption.get shouldBe an[IllegalArgumentException])

  "getPlayerStat" should "return the existing stat" in {
    val stat = existing(winner)
    fixture(stat).flatMap(_.service.getPlayerStat(winner, game)).asserting(_ shouldBe stat)
  }

  it should "return an empty stat when the player has none" in
    fixture().flatMap(_.service.getPlayerStat(winner, game)).asserting(_ shouldBe PlayerGameStat.Data(winner, game))

  "getPlayerStats" should "return all the stats of the player" in {
    val war       = existing(winner)
    val schnapsen = existing(winner, GameTyp.Schnapsen)
    fixture(war, schnapsen, existing(loser))
      .flatMap(_.service.getPlayerStats(winner))
      .asserting(_ shouldBe Some(PlayerGameStats(winner, NonEmptyList.of(war, schnapsen))))
  }

  it should "return nothing when the player has no stats" in
    fixture().flatMap(_.service.getPlayerStats(winner)).asserting(_ shouldBe None)

  "readAll" should "stream all the stats" in {
    val stats = List(existing(winner), existing(loser))
    fixture(stats*).flatMap(_.service.readAll.compile.toList).asserting(_ shouldBe stats)
  }

  "delete" should "remove only the given stat" in {
    val stat  = existing(winner)
    val other = existing(loser)
    for
      f      <- fixture(stat, other)
      _      <- f.service.delete(stat)
      stored <- f.stats.get
    yield stored shouldBe List(other)
  }

  "deleteMany" should "remove the given stats" in {
    val stat1 = existing(winner)
    val stat2 = existing(drawer)
    val other = existing(loser)
    for
      f      <- fixture(stat1, stat2, other)
      _      <- f.service.deleteMany(NonEmptyList.of(stat1, stat2))
      stored <- f.stats.get
    yield stored shouldBe List(other)
  }

  "deleteAll" should "remove all the stats" in {
    for
      f      <- fixture(existing(winner), existing(loser))
      _      <- f.service.deleteAll
      stored <- f.stats.get
    yield stored shouldBe empty
  }

  "GameStatRepository.insert" should "not force an id by default" in {
    val data = PlayerGameStat.Data(winner, game)
    fixture().flatMap(_.repo.insert(data)).asserting(_.data shouldBe data)
  }

  "PlayerGameStat.Data" should "be displayed" in {
    val stat = PlayerGameStat.Data(winner, game, 1, 2, 3)
    stat.show shouldBe s"playerId = $winner | game = war | played = 6 | won = 1 | draw = 2 | lost = 3"
  }

  "PlayerGameStat.Existing" should "be displayed" in {
    val stat = existing(winner)
    stat.show should include(s"id = ${stat.id}")
    stat.show should include(s"playerId = $winner")
  }
