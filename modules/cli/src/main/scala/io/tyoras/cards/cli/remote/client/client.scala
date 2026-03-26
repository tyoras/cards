package io.tyoras.cards.cli.remote.client

import cats.data.NonEmptyList
import cats.effect.{Async, Resource}
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.cli.remote.auth.AuthProvider
import io.tyoras.cards.domain.game.GameType
import org.http4s.{AuthScheme, Credentials, Headers, MediaType}
import org.http4s.client.Client
import org.http4s.client.middleware.{Logger, Retry, RetryPolicy}
import org.http4s.headers.{Accept, Authorization}
import org.http4s.client.middleware.RetryPolicy.exponentialBackoff

import java.time.ZonedDateTime
import scala.concurrent.duration.DurationInt

final case class Game(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, gameType: GameType, players: NonEmptyList[FUUID])

def authedClient[F[_] : Async](underlying: Client[F], authProvider: AuthProvider[F]): Client[F] =
  val retryPolicy = RetryPolicy[F](backoff = exponentialBackoff(3.second, maxRetry = 5))
  val loggedClient = Logger.colored[F](logHeaders = true, logBody = true)(Client[F] { req =>
    for
      creds <- Resource.eval(authProvider.connectedUserCredentials)
      result <- underlying.run(
        req.putHeaders(
          Headers(
            Authorization(Credentials.Token(AuthScheme.Bearer, creds.token.value)),
            Accept(MediaType.application.json)
          )
        )
      )
    yield result
  })
  Retry(retryPolicy)(loggedClient)
