package io.tyoras.cards.domain.healthcheck

import cats.Parallel
import cats.effect.Async
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import io.tyoras.cards.domain.healthcheck.CheckType.{Liveness, Readiness, Startup}
import io.tyoras.cards.domain.healthcheck.Status.*

trait HealthMonitorService[F[_]]:
  def startup: F[HealthStatus]
  def readiness: F[HealthStatus]
  def liveness: F[HealthStatus]

object HealthMonitorService:
  def of[F[_] : Async : Parallel](startupProbes: List[Probe[F]], readinessProbes: List[Probe[F]], livenessProbes: List[Probe[F]]): F[HealthMonitorService[F]] =
    for started <- AtomicCell[F].of(HealthStatus.ComponentHealth.down(Startup.name))
    yield new HealthMonitorService[F]:
      override lazy val startup: F[HealthStatus] = started.evalUpdateAndGet { healthStatus =>
        healthStatus.status match
          case UP => healthStatus.pure
          case _  => startupProbes.parTraverse(_.check).map(HealthStatus.of(Startup.name, _))
      }

      override lazy val readiness: F[HealthStatus] =
        startup.map(_.status).flatMap {
          case DOWN => HealthStatus.ComponentHealth.down(Readiness.name).pure
          case _    => readinessProbes.parTraverse(_.check).map(HealthStatus.of(Readiness.name, _, defaultStatus = DOWN))
        }

      override lazy val liveness: F[HealthStatus] =
        startup.map(_.status).flatMap {
          case DOWN => HealthStatus.ComponentHealth.down(Liveness.name).pure
          case _    => livenessProbes.parTraverse(_.check).map(HealthStatus.of(Liveness.name, _, defaultStatus = DOWN))
        }
