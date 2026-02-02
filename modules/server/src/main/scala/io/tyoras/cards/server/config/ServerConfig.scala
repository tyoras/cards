package io.tyoras.cards.server.config

import io.tyoras.cards.persistence.DatabaseConfig

final case class ServerConfig(http: HttpConfig, database: DatabaseConfig)

final case class HttpConfig(host: String, port: Int)
