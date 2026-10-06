package io.tyoras.cards.persistence

import cats.effect.{IO, Resource}
import io.tyoras.cards.domain.healthcheck.Status.UP
import io.tyoras.cards.domain.healthcheck.{Probe, Status}
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import scala.concurrent.duration.DurationInt

class DbProbeSpec extends PgIntegrationTest:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def testedProbe(probeName: String): Resource[IO, Probe[IO]] = DbProbe.of[IO](probeName, sessionPool)
  "DbProbe" should {
    "return UP status when database is reachable" in {
      testedProbe("test_db_probe").use { probe =>
        for
          _      <- IO.sleep(500.millis) // Wait for the first scheduled check to complete
          result <- probe.check
        yield {
          probe.name shouldBe "test_db_probe"
          result.name shouldBe probe.name
          result.status shouldBe UP
        }
      }
    }
  }
