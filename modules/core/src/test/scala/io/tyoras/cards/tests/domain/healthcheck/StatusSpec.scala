package io.tyoras.cards.tests.domain.healthcheck

import cats.data.NonEmptyList
import io.tyoras.cards.domain.healthcheck.HealthStatus.ComponentHealth
import io.tyoras.cards.domain.healthcheck.Status.{DEGRADED, DOWN, UP}
import io.tyoras.cards.domain.healthcheck.{HealthStatus, Status}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StatusSpec extends AnyWordSpec with Matchers:
  "of" should {
    "return UP status when no components are provided" in {
      val result = HealthStatus.of("test")
      result.status shouldBe UP
      result.components shouldBe empty
    }

    "return DOWN status when at least one component is DOWN" in {
      val downComponent = ComponentHealth("downComponent", DOWN)
      val upComponent   = ComponentHealth("upComponent", UP)
      val result        = HealthStatus.of("test", List(downComponent, upComponent))
      result.status shouldBe DOWN
    }

    "return DEGRADED status when at least one component is DEGRADED and none are DOWN" in {
      val degradedComponent = ComponentHealth("degradedComponent", DEGRADED)
      val upComponent       = ComponentHealth("upComponent", UP)
      val result            = HealthStatus.of("test", List(degradedComponent, upComponent))
      result.status shouldBe DEGRADED
    }

    "return UP status when all components are UP" in {
      val upComponent1 = ComponentHealth("upComponent1", UP)
      val upComponent2 = ComponentHealth("upComponent2", UP)
      val result       = HealthStatus.of("test", List(upComponent1, upComponent2))
      result.status shouldBe UP
    }
  }
  "ComponentHealth" should {
    "return the correct name and status" in {
      val component = ComponentHealth("testComponent", UP)
      component.name shouldBe "testComponent"
      component.status shouldBe UP
      component.components shouldBe empty
    }
  }
  "CompositeHealth" should {
    "return the correct name, status, and components" in {
      val component1 = ComponentHealth("component1", UP)
      val component2 = ComponentHealth("component2", DOWN)
      val composite  = HealthStatus.CompositeHealth("composite", NonEmptyList.of(component1, component2))
      composite.name shouldBe "composite"
      composite.status shouldBe DOWN
      composite.components shouldBe List(component1, component2)
    }
  }
