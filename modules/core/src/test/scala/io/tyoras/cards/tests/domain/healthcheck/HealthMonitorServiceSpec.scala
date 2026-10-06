package io.tyoras.cards.tests.domain.healthcheck

import cats.data.NonEmptyList
import cats.effect.std.AtomicCell
import cats.effect.testing.scalatest.AsyncIOSpec
import io.tyoras.cards.domain.healthcheck.{HealthMonitorService, HealthStatus, Probe, Status}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import cats.effect.IO
import io.tyoras.cards.domain.healthcheck.HealthStatus.CompositeHealth

object HealthMonitorServiceSpec:
  val allUpProbes: IO[List[Probe.Manual[IO]]] = List(
    probe("probe1", Status.UP),
    probe("probe2", Status.UP),
    probe("probe3", Status.UP)
  ).sequence

  def probe(name: String, initStatus: Status): IO[Probe.Manual[IO]] =
    AtomicCell[IO].of(initStatus).map(Probe.Manual(_, name))

class HealthMonitorServiceSpec extends AsyncWordSpec with AsyncIOSpec with Matchers:
  "HealthMonitorService" when {
    "all startup probes are UP" should {
      "return UP status and stay up after even if individual probes change status" in {
        for
          startupProbes   <- HealthMonitorServiceSpec.allUpProbes
          tested          <- HealthMonitorService.of[IO](startupProbes, Nil, Nil)
          result          <- tested.startup
          _               <- startupProbes(1).down
          resultAfterDown <- tested.startup
        yield
          startupProbes should not be empty
          result shouldBe CompositeHealth("startup", NonEmptyList.fromListUnsafe(startupProbes.map(p => HealthStatus.ComponentHealth(p.name, Status.UP))))
          result.status shouldBe Status.UP
          resultAfterDown.status shouldBe Status.UP
      }
    }
    "at least one startup probe is DOWN" should {
      "return DOWN status" in {
        for
          startupProbes <- HealthMonitorServiceSpec.allUpProbes
          _             <- startupProbes(1).down
          tested        <- HealthMonitorService.of[IO](startupProbes, Nil, Nil)
          result        <- tested.startup
        yield result.status shouldBe Status.DOWN
      }
    }
    "there are no startup probes" should {
      "return UP status" in {
        for
          tested <- HealthMonitorService.of[IO](Nil, Nil, Nil)
          result <- tested.startup
        yield result.status shouldBe Status.UP
      }
    }
    "startup health status is DOWN" should {
      "return DOWN status for readiness and liveness when they are empty" in {
        for
          startupProbes   <- HealthMonitorServiceSpec.allUpProbes
          _               <- startupProbes(1).down
          tested          <- HealthMonitorService.of[IO](startupProbes, Nil, Nil)
          startupResult   <- tested.startup
          readinessResult <- tested.readiness
          livenessResult  <- tested.liveness
        yield
          startupResult.status shouldBe Status.DOWN
          readinessResult.status shouldBe Status.DOWN
          livenessResult.status shouldBe Status.DOWN
      }

      "return DOWN status for readiness and liveness when they have probes and are all UP" in {
        for
          startupProbes   <- HealthMonitorServiceSpec.allUpProbes
          readinessProbes <- HealthMonitorServiceSpec.allUpProbes
          livenessProbes  <- HealthMonitorServiceSpec.allUpProbes
          _               <- startupProbes(1).down
          tested          <- HealthMonitorService.of[IO](startupProbes, readinessProbes, livenessProbes)
          startupResult   <- tested.startup
          readinessResult <- tested.readiness
          livenessResult  <- tested.liveness
        yield
          startupResult.status shouldBe Status.DOWN
          readinessResult.status shouldBe Status.DOWN
          livenessResult.status shouldBe Status.DOWN
      }
    }
    "startup health status is UP" should {
      "return DOWN status for readiness when at least one probe is DOWN" in {
        for
          startupProbes   <- HealthMonitorServiceSpec.allUpProbes
          readinessProbes <- HealthMonitorServiceSpec.allUpProbes
          _               <- readinessProbes(1).down
          tested          <- HealthMonitorService.of[IO](startupProbes, readinessProbes, Nil)
          startupResult   <- tested.startup
          readinessResult <- tested.readiness
        yield
          startupResult.status shouldBe Status.UP
          readinessResult.status shouldBe Status.DOWN
      }

      "return DOWN status for liveness when at least one probe is DOWN" in {
        for
          startupProbes  <- HealthMonitorServiceSpec.allUpProbes
          livenessProbes <- HealthMonitorServiceSpec.allUpProbes
          _              <- livenessProbes(1).down
          tested         <- HealthMonitorService.of[IO](startupProbes, Nil, livenessProbes)
          startupResult  <- tested.startup
          livenessResult <- tested.liveness
        yield
          startupResult.status shouldBe Status.UP
          livenessResult.status shouldBe Status.DOWN
      }

      "return UP status for readiness and liveness when all probes are UP" in {
        for
          startupProbes   <- HealthMonitorServiceSpec.allUpProbes
          readinessProbes <- HealthMonitorServiceSpec.allUpProbes
          livenessProbes  <- HealthMonitorServiceSpec.allUpProbes
          tested          <- HealthMonitorService.of[IO](startupProbes, readinessProbes, livenessProbes)
          startupResult   <- tested.startup
          readinessResult <- tested.readiness
          livenessResult  <- tested.liveness
        yield
          startupResult.status shouldBe Status.UP
          readinessResult.status shouldBe Status.UP
          livenessResult.status shouldBe Status.UP
      }
    }
  }
