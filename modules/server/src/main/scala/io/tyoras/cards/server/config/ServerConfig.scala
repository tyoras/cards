package io.tyoras.cards.server.config

import io.tyoras.cards.domain.auth.config.AuthConfig
import io.tyoras.cards.persistence.config.DatabaseConfig
import org.http4s.headers.Origin
import pureconfig.ConfigReader

final case class ServerConfig(http: HttpConfig, database: DatabaseConfig, auth: AuthConfig) derives ConfigReader

final case class HttpConfig(host: String, port: Int, corsAllowedOrigins: Set[Origin.Host])
