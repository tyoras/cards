package io.tyoras.cards.persistence

import cats.effect.std.Console
import cats.effect.{Concurrent, Resource, Sync}
import cats.syntax.all._
import fs2.io.net.Network
import io.tyoras.cards.config.DatabaseConfig
import natchez.Trace
import org.flywaydb.core.Flyway
import skunk.{Session, SessionPool}

object SessionPool {
  def of[F[_] : Sync : Concurrent : Trace : Network : Console](config: DatabaseConfig): SessionPool[F] = {
    Resource.eval(initializeDb(config)) >>
      Session.pooled(
        host = config.host,
        port = config.port,
        user = config.user,
        password = config.password.some,
        database = config.db,
        max = config.maxSession,
        debug = false
      )
  }

  def initializeDb[F[_] : Sync](config: DatabaseConfig): F[Unit] =
    F.delay {
      Flyway.configure().dataSource(config.jdbcUrl, config.user, config.password).load().migrate()
    }.void
}
