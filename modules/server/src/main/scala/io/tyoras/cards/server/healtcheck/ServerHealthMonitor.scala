package io.tyoras.cards.server.healtcheck

import cats.Parallel
import cats.effect.{Async, Resource}
import cats.effect.std.AtomicCell
import io.tyoras.cards.domain.healthcheck.{HealthMonitorService, Probe, Status}
import cats.syntax.all.*
import io.tyoras.cards.persistence.DbProbe
import org.typelevel.log4cats.LoggerFactory
import skunk.Session

trait ServerHealthMonitor[F[_]]:
  def healthMonitor: HealthMonitorService[F]
  def serverLivenessProbe: Probe.Manual[F]
  def dbMigrationProgressProbe: Probe.Manual[F]

object ServerHealthMonitor:
  def of[F[_] : Async : Parallel : LoggerFactory](dbSessionPool: Resource[F, Session[F]]): Resource[F, ServerHealthMonitor[F]] =

    for
      serverAlive <- Resource.eval(AtomicCell[F].of(Status.DOWN).map(Probe.Manual(_, "server")))
      dbMigration <- Resource.eval(AtomicCell[F].of(Status.UP).map(Probe.Manual(_, "db_migration")))
      dbProbe     <- DbProbe.of[F]("db", dbSessionPool)
      monitor <- Resource.eval(HealthMonitorService.of(startupProbes = List(dbMigration), readinessProbes = List(dbProbe), livenessProbes = List(serverAlive)))
    yield new ServerHealthMonitor[F]:
      override def healthMonitor: HealthMonitorService[F]    = monitor
      override def serverLivenessProbe: Probe.Manual[F]      = serverAlive
      override def dbMigrationProgressProbe: Probe.Manual[F] = dbMigration
