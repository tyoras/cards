package io.tyoras.cards.config

final case class CardsConfig(server: ServerConfig, database: DatabaseConfig)

final case class ServerConfig(host: String, port: Int)
