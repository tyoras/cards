package io.tyoras.cards.server

import cats.effect.*
import cats.effect.kernel.Resource
import cats.effect.std.Console
import fs2.io.net.Network
import io.tyoras.cards.server.config.*
import io.tyoras.cards.domain.game.GameService
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.persistence.game.PostgresGameRepository
import io.tyoras.cards.persistence.user.PostgresUserRepository
import io.tyoras.cards.persistence.SessionPool
import io.tyoras.cards.server.endpoints.games.GameEndpoint
import io.tyoras.cards.server.endpoints.games.war.WarEndpoint
import io.tyoras.cards.server.endpoints.users.UserEndpoint
import natchez.Trace.Implicits.noop

import java.nio.file.{Path, Paths}

object Main extends IOApp:
  private val defaultConfigPath = Paths.get("cards-server.conf")
  override def run(args: List[String]): IO[ExitCode] =
    val configPath = args.headOption.fold(defaultConfigPath)(Paths.get(_))
    init[IO](configPath).useForever.as(ExitCode.Success).handleErrorWith(t => IO.println(s"Service has failed to start ${t.getMessage}").as(ExitCode.Error))

  private def init[F[_] : Async : Console : Network : natchez.Trace](configPath: Path): Resource[F, Unit] = for
    config        <- Resource.eval(parseConfig(configPath))
    dbSessionPool <- SessionPool.of(config.database)
    userRepo      <- Resource.eval(PostgresUserRepository.of[F](dbSessionPool))
    userService = UserService.of(userRepo)
    gameRepo <- Resource.eval(PostgresGameRepository.of[F](dbSessionPool))
    gameService = GameService.of(gameRepo)
    userEndpoint <- Resource.eval(UserEndpoint.of(userService))
    gameEndpoint <- Resource.eval(GameEndpoint.of(gameService))
    warEndpoint  <- Resource.eval(WarEndpoint.of(gameService))
    httpApp = Server.HttpApp.of(userEndpoint, gameEndpoint, warEndpoint)
    _ <- Server.of(config.http, httpApp).serve
  yield ()
