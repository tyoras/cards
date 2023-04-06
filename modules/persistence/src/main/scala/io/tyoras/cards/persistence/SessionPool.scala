package io.tyoras.cards.persistence

import cats.effect.std.Console
import cats.effect.{Concurrent, Resource, Sync, Temporal}
import cats.syntax.all.*
import fs2.io.net.Network
import io.tyoras.cards.config.DatabaseConfig
import natchez.Trace
import org.flywaydb.core.Flyway
import skunk.{Session, SessionPool, Strategy}

object SessionPool:
  def of[F[_] : Sync : Temporal : Trace : Network : Console](config: DatabaseConfig): SessionPool[F] =
    Resource
      .eval(initializeDb(config))
      .flatMap(_ =>
        Session.pooled(
          host = config.host,
          port = config.port,
          user = config.user,
          password = config.password.some,
          database = config.db,
          max = config.maxSession,
          strategy = Strategy.SearchPath,
          debug = false
        )
      )

  private def initializeDb[F[_] : Sync](config: DatabaseConfig): F[Unit] =
    Sync[F].delay {
      Flyway.configure().dataSource(config.jdbcUrl, config.user, config.password).load().migrate()
    }.void
