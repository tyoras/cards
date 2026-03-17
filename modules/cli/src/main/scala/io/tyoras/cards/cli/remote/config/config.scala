package io.tyoras.cards.cli.remote.config

import cats.effect.kernel.Sync
import org.http4s.implicits.*

def parseConfig[F[_] : Sync](configPath: String): F[RemoteCliConfig] =
  Sync[F].pure(
    RemoteCliConfig(
      client = CardsClientConfig(
        apiUri = uri"http://localhost:8080",
        wsUri = uri"ws://localhost:8080"
      ),
      auth = AuthConfig(
        userName = "Yo",
        password = "fake_password"
      )
    )
  )
