package io.tyoras.cards.persistence

import cats.effect.*
import cats.effect.implicits.*
import io.tyoras.cards.domain.healthcheck.{HealthStatus, Probe, Status}
import cats.syntax.all.*
import io.tyoras.cards.domain.healthcheck.Status.*
import skunk.*
import skunk.implicits.*
import skunk.codec.all.*

import scala.concurrent.duration.*
import fs2.Stream
import org.typelevel.log4cats.LoggerFactory

object DbProbe:
  private val query = sql"SELECT 1 + 1".query(int4)
  def of[F[_] : Temporal : LoggerFactory](probeName: String, sessionPool: Resource[F, Session[F]]): Resource[F, Probe.Automatic[F]] =
    for
      logger    <- Resource.eval(LoggerFactory[F].create)
      status    <- Resource.eval(Ref[F].of(DOWN))
      scheduled <- Stream
        .fixedRateStartImmediately(5.seconds)
        .evalMap(_ => sessionPool.use(_.unique(query)).timeout(1.second).attempt.map(_.fold(_ => DOWN, _ => UP)))
        .evalTap(computedStatus => logger.debug(s"Database probe '$probeName' status updated to $computedStatus"))
        .evalTap(status.set)
        .compile
        .drain
        .background
    yield new Probe.Automatic[F]:
      override def name: String           = probeName
      override def check: F[HealthStatus] = status.get.map(HealthStatus.ComponentHealth(name, _))
