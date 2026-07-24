package io.tyoras.cards.cli.remote.config

import io.tyoras.cards.domain.auth.model.Password
import io.tyoras.cards.domain.user.model.User
import org.http4s.Uri
import pureconfig.ConfigReader
import pureconfig.module.http4s.given
import io.github.iltotore.iron.pureconfig.given

given ConfigReader[User.Name] = summon
given ConfigReader[Password]  = summon
final case class RemoteCliConfig(client: CardsClientConfig, auth: AuthConfig) derives ConfigReader
final case class CardsClientConfig(apiUri: Uri, wsUri: Uri)
final case class AuthConfig(userName: User.Name, password: Password) derives ConfigReader
