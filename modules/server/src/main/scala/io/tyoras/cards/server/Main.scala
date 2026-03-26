package io.tyoras.cards.server

import cats.effect.*
import cats.effect.kernel.Resource
import cats.effect.std.Console
import fs2.io.file.Files
import fs2.io.net.Network
import io.tyoras.cards.domain.auth.{AuthService, JWTGenerator, JwtExpiration}
import io.tyoras.cards.server.config.*
import io.tyoras.cards.domain.game.GameService
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.persistence.game.PostgresGameRepository
import io.tyoras.cards.persistence.user.PostgresUserRepository
import io.tyoras.cards.persistence.SessionPool
import io.tyoras.cards.server.endpoints.auth.AuthEndpoint
import io.tyoras.cards.server.endpoints.chat.ChatEndpoint
import io.tyoras.cards.server.endpoints.games.GameEndpoint
import io.tyoras.cards.server.endpoints.games.war.WarEndpoint
import io.tyoras.cards.server.endpoints.users.UserEndpoint
import io.tyoras.cards.server.protocol.chat.ChatProtocol
import io.tyoras.cards.server.protocol.game.GameProtocol
import natchez.Trace.Implicits.noop
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import pureconfig.ConfigSource

import java.nio.file.{Path, Paths}

object Main extends IOApp:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val defaultConfigSource = ConfigSource.resources("cards-server.conf")
  override def run(args: List[String]): IO[ExitCode] =
    val configSource = args.headOption.fold(defaultConfigSource)(ConfigSource.file)
    init[IO](configSource).useForever
      .as(ExitCode.Success)
      .handleErrorWith(t => Console[IO].errorln(s"Service has failed to start ${t.getMessage}").as(ExitCode.Error))

  private def init[F[_] : Async : Console : Network : Files : natchez.Trace : LoggerFactory](configSource: ConfigSource): Resource[F, Unit] =
    for
      config        <- Resource.eval(parseConfig(configSource))
      dbSessionPool <- SessionPool.of(config.database)
      userRepo      <- Resource.eval(PostgresUserRepository.of[F](dbSessionPool))
      userService = UserService.of(userRepo)
      jwtExpiration <- Resource.eval(JwtExpiration.make)
      jwtGenerator = JWTGenerator.make(jwtExpiration, config.auth)
      gameRepo <- Resource.eval(PostgresGameRepository.of[F](dbSessionPool))
      gameService = GameService.of(gameRepo)
      // FIXME usage of insecure naive auth
      authService  <- Resource.eval(AuthService.naive(userService, jwtGenerator, config.auth))
      chatProtocol <- Resource.eval(ChatProtocol.make(authService))
      gameProtocol <- Resource.eval(GameProtocol.make(authService))
      userEndpoint <- Resource.eval(UserEndpoint.of(userService))
      gameEndpoint <- Resource.eval(GameEndpoint.of(gameService))
      warEndpoint  <- WarEndpoint.make(gameService, userService, gameProtocol, chatProtocol)
      authEndpoint <- Resource.eval(AuthEndpoint.of(authService))
      chatEndpoint <- ChatEndpoint.make(chatProtocol)
      httpWsApp = Server.HttpWsApp.of(config.auth, authService)(authEndpoint, userEndpoint, gameEndpoint, warEndpoint, authEndpoint, chatEndpoint)
      _ <- Server.of(config.http, httpWsApp).serve
    yield ()
