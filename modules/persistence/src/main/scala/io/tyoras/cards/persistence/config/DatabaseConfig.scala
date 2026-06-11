package io.tyoras.cards.persistence.config

final case class DatabaseConfig(host: String, port: Int, user: String, password: String, db: String, maxSession: Int):
  lazy val jdbcUrl: String = s"jdbc:postgresql://$host:$port/"
