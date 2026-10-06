package io.tyoras.cards.tests.domain.healthcheck

import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.effect.testing.scalatest.AsyncIOSpec
import io.tyoras.cards.domain.healthcheck.{Probe, Status}
import io.tyoras.cards.tests.domain.healthcheck.ProbeSpec.{manualProbe, probeName}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

object ProbeSpec:
  def manualProbe(name: String, initialStatus: Status): IO[Probe.Manual[IO]] =
    AtomicCell[IO].of(initialStatus).map(Probe.Manual(_, name))

  val probeName = "test-probe"

class ProbeSpec extends AsyncWordSpec with AsyncIOSpec with Matchers:
  "Probe" when {
    "Manual probe" should {
      "return the correct name and status" in {
        val initialStatus = Status.UP
        for
          probe  <- manualProbe(probeName, initialStatus)
          result <- probe.check
        yield
          probe.name shouldBe probeName
          result.name shouldBe probeName
          result.status shouldBe initialStatus
      }
    }

    "change status correctly" in {
      val initialStatus = Status.UP
      for
        probe               <- manualProbe(probeName, initialStatus)
        _                   <- probe.down
        resultAfterDown     <- probe.check
        _                   <- probe.up
        resultAfterUp       <- probe.check
        _                   <- probe.degraded
        resultAfterDegraded <- probe.check
      yield
        resultAfterDown.status shouldBe Status.DOWN
        resultAfterUp.status shouldBe Status.UP
        resultAfterDegraded.status shouldBe Status.DEGRADED
    }
  }
