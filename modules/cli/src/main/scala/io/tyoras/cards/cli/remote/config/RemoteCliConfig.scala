package io.tyoras.cards.cli.remote.config

import io.tyoras.cards.domain.auth.{Password, UserName}
import org.http4s.Uri
import pureconfig.ConfigReader
import pureconfig.module.http4s.given

final case class RemoteCliConfig(client: CardsClientConfig, auth: AuthConfig) derives ConfigReader
final case class CardsClientConfig(apiUri: Uri, wsUri: Uri)
final case class AuthConfig(userName: UserName, password: Password)
