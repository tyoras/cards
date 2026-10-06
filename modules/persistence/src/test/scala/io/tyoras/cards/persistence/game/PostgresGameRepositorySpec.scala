package io.tyoras.cards.persistence.game

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.card.Hand
import io.tyoras.cards.domain.game.war.model.{GameContext, GameState, Player, Turn}
import io.tyoras.cards.domain.game.{Game, GameRepository, GameTyp}
import io.tyoras.cards.domain.user.UserRepository
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.persistence.{PersistenceError, PgIntegrationTest}
import io.tyoras.cards.persistence.game.PostgresGameRepositorySpec.*
import io.tyoras.cards.persistence.user.PostgresUserRepository
import org.scalatest.BeforeAndAfterEach
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.time.ZonedDateTime
import java.util.UUID

object PostgresGameRepositorySpec:
  private val playerId       = FUUID.fromUUID(UUID.randomUUID())
  private val playerData     = User.Data(User.Name("Julio"), User.About("Cool cat"))
  private val existingPlayer = User.Existing(playerId, ZonedDateTime.now(), ZonedDateTime.now(), playerData)
  private val gameId         = FUUID.fromUUID(UUID.randomUUID())
  private val gameType       = GameTyp.War
  private val now            = ZonedDateTime.now()
  private val gameData       = Game.Data(
    gameType,
    NonEmptyList.one(playerId),
    GameState.Exit(GameContext(Map(playerId -> Player(playerId, Hand.empty)), now, Turn.assume(10), Nil)),
    playerId,
    None
  )

class PostgresGameRepositorySpec extends PgIntegrationTest with BeforeAndAfterEach:

  import gameType.given
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def userRepo: IO[UserRepository[IO]] = PostgresUserRepository.of(sessionPool)

  private def initPgRepo: IO[GameRepository[IO]] = PostgresGameRepository.of(sessionPool)

  override protected def beforeEach(): Unit =
    // cascading delete will remove everything else
    userRepo.flatMap(_.deleteAll).unsafeRunSync()

  "PostgresGameRepository" when {
    "inserting a game" when {
      "the game does not already exist" should {
        "be able to create a single game without providing an id" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo.flatMap(_.insert(gameData, withId = None)).asserting { created =>
              created.data shouldBe gameData
              created.createdAt shouldBe created.updatedAt
            }
        }

        "be able to create a single game with a provided id" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo.flatMap(_.insert(gameData, withId = gameId.some)).asserting { created =>
              created.data shouldBe gameData
              created.id shouldBe gameId
              created.createdAt shouldBe created.updatedAt
            }
        }
      }
      "the game already exist" should {
        "work when the id is not provided" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            (for
              testedRepo <- initPgRepo
              game1      <- testedRepo.insert(gameData, withId = None)
              game2      <- testedRepo.insert(gameData, withId = None)
            yield
              game1.data shouldBe gameData
              game2.data shouldBe gameData
              game1.id shouldNot be(game2.id))
        }

        "return an error (id provided)" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo
              .flatMap { testedRepo =>
                testedRepo.insert(gameData, withId = gameId.some) *>
                  testedRepo.insert(gameData, withId = gameId.some)
              }
              .assertThrowsError(_ shouldBe PersistenceError("already_exist", s"Game $gameId already exist"))
        }
      }
    }
    "updating a game" when {
      "the game does not already exist" should {
        "return an error" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo
              .flatMap { testedRepo =>
                testedRepo.update(Game.Existing(gameId, now, now, gameData))
              }
              .assertThrowsError(_ shouldBe PersistenceError("update_failed", s"Failed to update game $gameId"))
        }
      }
      "the game already exist" should {
        "be able to update a single game" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo.flatMap { testedRepo =>
              for
                created <- testedRepo.insert(gameData, withId = None)
                updated <- testedRepo.update(created.copy(data = created.data.copy(players = NonEmptyList.of(existingPlayer.id))))
              yield
                updated.data.players shouldBe NonEmptyList.of(existingPlayer.id)
                updated.updatedAt should be > created.updatedAt
            }
        }
      }
    }
    "reading a game" when {
      "the game does not already exist" should {
        "return an empty result" in
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
          initPgRepo.flatMap(_.readManyById[gameType.State](List(gameId))).asserting(_ shouldBe Nil)
      }
      "the game already exist" should {
        "be able to read a single game by id" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo.flatMap { testedRepo =>
              for
                created <- testedRepo.insert(gameData, withId = None)
                read    <- testedRepo.readManyById[gameType.State](List(created.id))
              yield
                read should have size 1
                read.head shouldBe created
            }
        }
      }
    }
    "reading all games" when {
      "no game exist" should {
        "return an empty result" in
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
          initPgRepo.flatMap(_.readAll[gameType.State](finished = false).compile.toList).asserting(_ shouldBe Nil)
      }
      "some games already exist" should {
        "be able to read all games" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo.flatMap { testedRepo =>
              for
                created1 <- testedRepo.insert(gameData, withId = None)
                created2 <- testedRepo.insert(gameData.copy(players = NonEmptyList.of(existingPlayer.id)), withId = None)
                read     <- testedRepo.readAll[gameType.State](finished = false).compile.toList
              yield
                read should have size 2
                read should contain allElementsOf List(created1, created2)
            }
        }
      }
    }
    "reading all games for a user" when {
      "no game exist" should {
        "return an empty result" in
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
          initPgRepo.flatMap(_.readManyByUser[gameType.State](existingPlayer.id, finished = false)).asserting(_ shouldBe Nil)
      }
      "some games already exist" should {
        "be able to read all games for a user" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo.flatMap { testedRepo =>
              for
                created1 <- testedRepo.insert(gameData, withId = None)
                created2 <- testedRepo.insert(gameData.copy(players = NonEmptyList.of(existingPlayer.id)), withId = None)
                read     <- testedRepo.readManyByUser[gameType.State](existingPlayer.id, finished = false)
              yield
                read should have size 2
                read should contain allElementsOf List(created1, created2)
            }
        }
      }
    }
    "reading all finished games for a user" when {
      "no game exist" should {
        "return an empty result" in
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
          initPgRepo.flatMap(_.readManyByUser[gameType.State](existingPlayer.id, finished = true)).asserting(_ shouldBe Nil)
      }
      "some finished games already exist" should {
        "be able to read all finished games for a user" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo.flatMap { testedRepo =>
              for
                created1 <- testedRepo.insert(gameData.copy(state = GameState.Exit(gameData.state.context), finishedAt = now.some), withId = None)
                created2 <- testedRepo.insert(
                  gameData.copy(players = NonEmptyList.of(existingPlayer.id), state = GameState.Exit(gameData.state.context), finishedAt = now.some),
                  withId = None
                )
                read <- testedRepo.readManyByUser[gameType.State](existingPlayer.id, finished = true)
              yield
                read should have size 2
                read should contain allElementsOf List(created1, created2)
            }
        }
      }
    }
    "deleting all games" when {
      "no game exist" should {
        "return an empty result" in
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
          initPgRepo.flatMap(_.deleteAll).asserting(_ shouldBe ())
      }
      "some games already exist" should {
        "be able to delete all games" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo.flatMap { testedRepo =>
              for
                _    <- testedRepo.insert(gameData, withId = None)
                _    <- testedRepo.insert(gameData.copy(players = NonEmptyList.of(existingPlayer.id)), withId = None)
                _    <- testedRepo.deleteAll
                read <- testedRepo.readAll[gameType.State](finished = false).compile.toList
              yield read shouldBe Nil
            }
        }
      }
    }
    "deleting many games" when {
      "no game exist" should {
        "return an empty result" in
          initPgRepo.flatMap(_.deleteMany(List())).asserting(_ shouldBe ())
      }
      "some games already exist" should {
        "be able to delete many games" in {
          userRepo.flatMap(_.insert(existingPlayer.data, withId = existingPlayer.id.some)) *>
            initPgRepo.flatMap { testedRepo =>
              for
                created1 <- testedRepo.insert(gameData, withId = None)
                created2 <- testedRepo.insert(gameData.copy(players = NonEmptyList.of(existingPlayer.id)), withId = None)
                _        <- testedRepo.deleteMany(List(created1, created2))
                read     <- testedRepo.readAll[gameType.State](finished = false).compile.toList
              yield read shouldBe Nil
            }
        }
      }
    }
  }
