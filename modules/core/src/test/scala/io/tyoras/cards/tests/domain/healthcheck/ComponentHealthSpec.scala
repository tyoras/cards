package io.tyoras.cards.tests.domain.healthcheck

import io.tyoras.cards.domain.healthcheck.HealthStatus.ComponentHealth
import io.tyoras.cards.domain.healthcheck.Status.{DOWN, UP}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ComponentHealthSpec extends AnyWordSpec with Matchers:
  "ComponentHealth" should {
    "build an UP component" in {
      ComponentHealth.up("db") shouldBe ComponentHealth("db", UP)
    }

    "build a DOWN component" in {
      ComponentHealth.down("db") shouldBe ComponentHealth("db", DOWN)
    }
  }
