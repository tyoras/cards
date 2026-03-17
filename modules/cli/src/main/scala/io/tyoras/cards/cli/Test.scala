package io.tyoras.cards.cli

import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.IO.asyncForIO
import cats.effect.std.Console
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.cli.remote.auth.AuthProvider
import io.tyoras.cards.cli.remote.client.{AuthClient, GamesClient, UsersClient}
import io.tyoras.cards.cli.remote.config.*
import io.tyoras.cards.domain.game.war.model.GameState
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.shared.protocol.game.OutputMessage

import java.net.http.HttpClient
import org.http4s.jdkhttpclient.{JdkHttpClient, JdkWSClient}
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import scala.concurrent.duration.DurationInt

object Test extends IOApp:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  override def run(args: List[String]): IO[ExitCode] =
    for
      config <- parseConfig[IO]("cli.conf").onError(e => Console[IO].errorln(s"Failed to parse config: $e"))
      (http, webSocket) <- IO(HttpClient.newHttpClient()).map { httpClient =>
        (JdkHttpClient[IO](httpClient), JdkWSClient[IO](httpClient))
      }
      authClient = AuthClient.make(config.client, http)
      authProvider <- AuthProvider.make(config.auth, authClient)
      authedHttpClient = remote.client.authedClient(http, authProvider)
      usersClient      = UsersClient.make(config.client, authedHttpClient)
      gamesClient      = GamesClient.make(config.client, authedHttpClient, webSocket, authProvider)
      allUsers     <- usersClient.listAll
      _            <- Console[IO].println(s"All users: ${allUsers.map(u => s"${u.name}[id=${u.id}]").mkString("\n\t-", "\n\t-", "\n")}")
      julio        <- usersClient.findById(FUUID.fuuid("b94f4b7e-bd91-4773-96de-1e1cc63a05f1"))
      _            <- Console[IO].println(s"Julio: $julio")
      randomUserId <- FUUID.randomFUUID[IO]
      randomUser   <- usersClient.findByPartialName(randomUserId.toString)
      _            <- Console[IO].println(s"Random: $randomUser")
      warGame      <- gamesClient.createWarGame(NonEmptyList.fromListUnsafe(allUsers).map(_.id))
      allGames     <- gamesClient.findByUserId(julio.get.id)
      _            <- Console[IO].println(s"All games: ${allGames.map(g => s"${g.gameType}[id=${g.id}]").mkString("\n\t-", "\n\t-", "\n")}")
      _            <- Console[IO].println(s"game created: $warGame")
      _ <- gamesClient.connectWarGame(warGame.id).use { warClient =>
        for
          _ <- Console[IO].println(s"connected to game ${warClient.gameId}")
          fiber <- warClient.streamServerMessages
            .evalTap(m => Console[IO].println(s"received message:\n$m"))
            .evalTap {
              case OutputMessage.GameState(_, _, state) =>
                IO.fromEither(state.as[GameState]).flatMap {
                  case GameState.Init(_, _) => Console[IO].println("seen Init state")
                  case _                    => Console[IO].println("seen some other state")
                }
              case _ => IO.unit
            }
            .compile
            .drain
            .start
          _ <- warClient.ready >> IO.sleep(2.second)
          _ <- (warClient.quit >> Console[IO].println("Disconnected gracefully")).race(IO.sleep(2.second) >> Console[IO].println("disconnection timeout"))
          _ <- Console[IO].println("Stopping stream") >> fiber.join >> Console[IO].println("stream stopped")
        yield ()
      }
      removed <- gamesClient.removeById(warGame.id)
      _       <- Console[IO].println(s"removed: $removed")
    yield ExitCode.Success
