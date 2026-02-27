package io.tyoras.cards.server.config

import io.tyoras.cards.domain.auth.AuthConfig
import io.tyoras.cards.persistence.DatabaseConfig

final case class ServerConfig(http: HttpConfig, database: DatabaseConfig, auth: AuthConfig)

final case class HttpConfig(host: String, port: Int)
