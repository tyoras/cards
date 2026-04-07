package io.tyoras.cards.persistence

import cats.effect.std.Console
import cats.effect.{Resource, Sync, Temporal}
import cats.syntax.all.*
import fs2.io.net.Network
import io.tyoras.cards.persistence.flyway.NativeImageResourceProvider
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
      val base = Flyway
        .configure()
        .dataSource(config.jdbcUrl, config.user, config.password)
      val configured = if (System.getProperty("org.graalvm.nativeimage.imagecode") != null) {
        base.resourceProvider(NativeImageResourceProvider())
      } else {
        base.locations(s"classpath:db/migration")
      }
      configured.load().migrate()
    }.void
