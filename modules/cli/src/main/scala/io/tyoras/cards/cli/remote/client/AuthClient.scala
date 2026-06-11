package io.tyoras.cards.cli.remote.client

import cats.effect.Async
import io.tyoras.cards.cli.remote.config.CardsClientConfig
import org.http4s.{Method, Request}
import org.http4s.client.Client
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import io.tyoras.cards.domain.auth.model.{Password, UserName}
import io.tyoras.cards.shared.endpoint.auth.Payloads
import io.tyoras.cards.shared.endpoint.auth.Payloads.Response.SuccessfulLogin
import cats.syntax.all.*
import io.tyoras.cards.cli.remote.auth.AuthCredentials
import org.http4s.client.middleware.RetryPolicy.exponentialBackoff
import org.http4s.client.middleware.{Logger, Retry, RetryPolicy}

import scala.concurrent.duration.DurationInt

trait AuthClient[F[_]]:
  def login(userName: UserName, password: Password): F[AuthCredentials]

object AuthClient:
  def make[F[_] : Async](config: CardsClientConfig, httpClient: Client[F]): AuthClient[F] = {
    val retryPolicy = RetryPolicy[F](backoff = exponentialBackoff(3.second, maxRetry = 5))
    val retryClient = Logger.colored[F](logHeaders = true, logBody = false)(Retry(retryPolicy)(httpClient))
    (userName: UserName, password: Password) =>
      val loginRequest = Request[F](
        method = Method.POST,
        uri = config.apiUri / "auth" / "login"
      ).withEntity(
        Payloads.Request.Login(Some(userName), Some(password))
      )
      retryClient.expect[SuccessfulLogin](loginRequest).map(r => AuthCredentials(r.userId, r.token))
  }
