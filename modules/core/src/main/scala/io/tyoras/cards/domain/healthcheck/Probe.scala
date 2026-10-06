package io.tyoras.cards.domain.healthcheck

import cats.Functor
import cats.effect.std.AtomicCell
import cats.syntax.all.*

sealed trait Probe[F[_]]:
  def name: String
  def check: F[HealthStatus]

object Probe:
  trait Manual[F[_]] extends Probe[F]:
    def down: F[Unit]
    def up: F[Unit]
    def degraded: F[Unit]
  object Manual:
    def apply[F[_] : Functor](status: AtomicCell[F, Status], probeName: String): Probe.Manual[F] =
      new Probe.Manual[F]:
        override def name: String           = probeName
        override def check: F[HealthStatus] = status.get.map(HealthStatus.ComponentHealth(name, _))
        override def down: F[Unit]          = status.set(Status.DOWN)
        override def up: F[Unit]            = status.set(Status.UP)
        override def degraded: F[Unit]      = status.set(Status.DEGRADED)

  trait Automatic[F[_]] extends Probe[F]

enum CheckType(val name: String):
  case Startup   extends CheckType("startup")
  case Readiness extends CheckType("readiness")
  case Liveness  extends CheckType("liveness")
