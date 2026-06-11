package io.tyoras.cards.persistence

import cats.effect.std.Console
import cats.effect.{Resource, Sync, Temporal}
import cats.syntax.all.*
import fs2.io.net.Network
import io.tyoras.cards.persistence.config.DatabaseConfig
import io.tyoras.cards.persistence.flyway.NativeImageResourceProvider
import org.flywaydb.core.Flyway
import org.typelevel.otel4s.metrics.Meter
import org.typelevel.otel4s.trace.Tracer
import skunk.Session
import skunk.TypingStrategy.SearchPath

object SessionPool:
  def of[F[_] : Sync : Meter : Tracer : Temporal : Network : Console](config: DatabaseConfig): Resource[F, Resource[F, Session[F]]] =
    Resource
      .eval(initializeDb(config))
      .flatMap(_ =>
        Session
          .Builder[F]
          .withHost(config.host)
          .withPort(config.port)
          .withUser(config.user)
          .withUserAndPassword(config.user, config.password)
          .withDatabase(config.db)
          .withDebug(false)
          .withTypingStrategy(SearchPath)
          .pooled(config.maxSession)
      )

  private def initializeDb[F[_] : Sync](config: DatabaseConfig): F[Unit] =
    Sync[F].delay {
      val base       = Flyway.configure().dataSource(config.jdbcUrl, config.user, config.password)
      val configured = if (System.getProperty("org.graalvm.nativeimage.imagecode") != null) {
        base.resourceProvider(NativeImageResourceProvider())
      } else {
        base.locations(s"classpath:db/migration")
      }
      configured.load().migrate()
    }.void
