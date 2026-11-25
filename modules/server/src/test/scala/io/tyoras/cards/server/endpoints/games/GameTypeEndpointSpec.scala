package io.tyoras.cards.server.endpoints.games

import cats.effect.{IO, Resource}
import io.tyoras.cards.domain.game.{Game, GameService}
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.server.endpoints.utils.{InMemoryUserService, TestAuthService, TestData, TestRoutes}
import io.tyoras.cards.server.endpoints.{Endpoint, EndpointSpec}
import io.tyoras.cards.server.protocol.chat.ChatProtocol
import io.tyoras.cards.server.protocol.fixtures.*
import io.tyoras.cards.server.protocol.game.GameProtocol
import org.http4s.HttpRoutes
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

/** Common setup for the game type specific endpoints (ERs, War...) specs: in memory services, a game protocol and the tested endpoint, all requests being
  * authenticated as [[creator]]
  */
trait GameTypeEndpointSpec extends EndpointSpec:
  protected given LoggerFactory[IO] = Slf4jFactory.create[IO]

  /** The authenticated user */
  protected val creator: User.Existing = TestData.existingUser
  protected val other: User.Existing   = user(newUserId, "other")
  protected val third: User.Existing   = user(newUserId, "third")

  /** Build the tested endpoint */
  protected def makeEndpoint(
      games: GameService[IO],
      users: UserService[IO],
      protocol: GameProtocol[IO],
      chat: ChatProtocol[IO]
  ): Resource[IO, Endpoint[IO]]

  /** Register an active game between the given players, returning its ID */
  protected def registerGame(env: Env, p1: User.ID, p2: User.ID): IO[Game.ID]

  protected case class Env(endpoint: Endpoint[IO], protocol: GameProtocol[IO], games: InMemoryGameService):
    val routes: HttpRoutes[IO] = TestRoutes.of(endpoint, creator)

  private lazy val env: Resource[IO, Env] =
    for
      games    <- Resource.eval(InMemoryGameService.make())
      stats    <- Resource.eval(RecordingGameStatService.make)
      chat     <- Resource.eval(ChatProtocol.make[IO](new TestAuthService))
      protocol <- GameProtocol.make[IO](new TestAuthService, games, stats)
      users    <- Resource.eval(InMemoryUserService.make(creator, other, third))
      endpoint <- makeEndpoint(games, users, protocol, chat)
    yield Env(endpoint, protocol, games)

  /** Run a test against a fresh environment, released afterward */
  protected def withEnv[A](test: Env => A): A =
    val (e, release) = env.allocated.unsafeRunSync()
    try test(e)
    finally release.unsafeRunSync()

  /** Shared environment, allocated once and kept alive for the whole spec (its finalizer is never run). Tests use [[withEnv]] instead. */
  override lazy val testedRoutes: HttpRoutes[IO] = env.allocated.unsafeRunSync()._1.routes
