package io.tyoras.cards.server.endpoints.games.ers

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import io.circe.syntax.*
import io.tyoras.cards.domain.game.ers.EgyptianRatscrew
import io.tyoras.cards.domain.game.ers.codecs.given
import io.tyoras.cards.domain.game.ers.model.ERsGameState
import io.tyoras.cards.domain.game.{Game, GameTyp}
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.server.endpoints.ErrorHandling.ApiError.InvalidRequest
import io.tyoras.cards.server.endpoints.utils.{TestAuthService, TestData}
import io.tyoras.cards.server.protocol.chat.ChatProtocol
import io.tyoras.cards.server.protocol.fixtures.*
import io.tyoras.cards.server.protocol.game.GameProtocol
import io.tyoras.cards.shared.endpoint.games.Payloads
import io.tyoras.cards.shared.endpoint.games.ers.Payloads.Request.Creation
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.implicits.*
import org.http4s.{AuthedRequest, Method, Request, Response, Status}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.util.UUID

class ERsEndpointSpec extends AnyFunSpec with Matchers:
  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val creator = TestData.existingUser
  private val other   = user(newUserId, "other")

  /** User service only able to read the given users */
  private class InMemoryUserService(users: User.Existing*) extends UserService[IO]:
    override def readById(id: User.ID): IO[Option[User.Existing]]                    = IO.pure(users.find(_.id == id))
    override def create(user: User.Data, withId: Option[User.ID]): IO[User.Existing] = IO.raiseError(new NotImplementedError)
    override def readByName(name: User.Name): IO[Option[User.Existing]]              = IO.pure(users.find(_.name == name))
    override def readManyById(ids: List[User.ID]): IO[List[User.Existing]]           = IO.pure(users.filter(u => ids.contains(u.id)).toList)
    override def readManyByPartialName(name: User.Name): IO[List[User.Existing]]     = IO.pure(Nil)
    override def readAll: Stream[IO, User.Existing]                                  = Stream.emits(users)
    override def update(user: User.Existing): IO[User.Existing]                      = IO.pure(user)
    override def delete(user: User.Existing): IO[Unit]                               = IO.unit
    override def deleteMany(users: List[User.Existing]): IO[Unit]                    = IO.unit
    override def deleteAll: IO[Unit]                                                 = IO.unit

  private case class Env(endpoint: Endpoint[IO], protocol: GameProtocol[IO], games: InMemoryGameService)

  private def withEndpoint[A](test: Env => IO[A]): A =
    (for
      games  <- InMemoryGameService.make()
      stats  <- RecordingGameStatService.make
      chat   <- ChatProtocol.make[IO](new TestAuthService)
      result <- GameProtocol.make[IO](new TestAuthService, games, stats).use { protocol =>
        ERsEndpoint.make[IO](games, new InMemoryUserService(creator, other), protocol, chat).use(e => test(Env(e, protocol, games)))
      }
    yield result).unsafeRunSync()

  private def run(env: Env, request: Request[IO]): IO[Response[IO]] =
    env.endpoint.authedRoutes.run(AuthedRequest(creator, request)).getOrElseF(IO.raiseError(new NoSuchElementException("route not found")))

  private def create(env: Env, players: NonEmptyList[User.ID]): IO[Response[IO]] =
    run(env, Request[IO](Method.POST, uri"/games/ers").withEntity(Creation(players).asJson))

  describe("POST /games/ers") {
    it("should create, store and activate a new Egyptian Ratscrew game") {
      val (status, game, stored, active) = withEndpoint { env =>
        for
          response <- create(env, NonEmptyList.of(creator.id, other.id))
          game     <- response.as[Payloads.Response.Game]
          stored   <- env.games.readById[ERsGameState](game.id)
          active   <- env.protocol.activeGames
        yield (response.status, game, stored, active)
      }
      status shouldBe Status.Created
      game.gameType shouldBe GameTyp.EgyptianRatscrew
      game.players shouldBe NonEmptyList.of(creator.id, other.id)
      stored.map(_.data.createdBy) shouldBe Some(creator.id)
      stored.map(_.state) match
        case Some(init: ERsGameState.Init) =>
          init.ready shouldBe empty
          init.context.players.keySet shouldBe Set(creator.id, other.id)
        case s => fail(s"Expected a stored Init state but got $s")
      active.get(game.id, GameTyp.EgyptianRatscrew) shouldBe defined
    }

    it("should reject a game with unknown players") {
      val result = withEndpoint(env => create(env, NonEmptyList.of(creator.id, newUserId)).attempt)
      result.left.toOption.get shouldBe an[InvalidRequest]
    }
  }

  describe("GET /games/ers/active") {
    it("should list only the active Egyptian Ratscrew games of the player") {
      val (created, listed) = withEndpoint { env =>
        for
          game <- create(env, NonEmptyList.of(creator.id, other.id)).flatMap(_.as[Payloads.Response.Game])
          ersState = ERsGameState.Init(ersTwoPlayersContext(creator.id, other.id))
          _        <- env.games.update(ersGame(game.id, ersState))
          ers      <- EgyptianRatscrew.fromState[IO](game.id, ersState)
          _        <- env.protocol.registerActiveGame(game.id, ers)
          response <- run(env, Request[IO](Method.GET, uri"/games/ers/active"))
          listed   <- response.as[List[UUID]]
        yield game.id -> listed
      }
      listed shouldBe List(created.unwrap)
    }
  }
