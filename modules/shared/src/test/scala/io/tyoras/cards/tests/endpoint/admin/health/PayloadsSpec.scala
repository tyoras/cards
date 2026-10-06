package io.tyoras.cards.tests.endpoint.admin.health

import io.tyoras.cards.shared.endpoint.admin.health.Payloads
import io.tyoras.cards.shared.endpoint.admin.health.Payloads.Response.HealthStatus.given
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.scalaland.chimney.dsl.*
import io.tyoras.cards.domain.healthcheck.HealthStatus
import io.tyoras.cards.domain.healthcheck.Status.{DOWN, UP}
import io.circe.syntax.*

class PayloadsSpec extends AnyWordSpec with Matchers with EitherValues:
  "HealthStatus transformer" should {
    "transform DomainHealthStatus to Response.HealthStatus correctly" in {
      val domainHealthStatus = HealthStatus.of(
        "test",
        List(
          HealthStatus.ComponentHealth("component1", UP),
          HealthStatus.ComponentHealth("component2", DOWN)
        )
      )

      val responseHealthStatus = domainHealthStatus.transformInto[Payloads.Response.HealthStatus]

      responseHealthStatus.name shouldBe "test"
      responseHealthStatus.status shouldBe "DOWN"
      (responseHealthStatus.components.map(_.name) should contain).allOf("component1", "component2")
      (responseHealthStatus.components.map(_.status) should contain).allOf("UP", "DOWN")
    }
  }
  "HealthStatus json codec" should {
    "encode and decode Response.HealthStatus correctly" in {
      val responseHealthStatus = Payloads.Response.HealthStatus(
        name = "test",
        status = "DOWN",
        components = List(
          Payloads.Response.HealthStatus("component1", "UP", Nil),
          Payloads.Response.HealthStatus("component2", "DOWN", Nil)
        )
      )

      val json    = io.circe.parser.parse(responseHealthStatus.asJson.noSpaces).getOrElse(fail("Failed to parse JSON"))
      val decoded = json.as[Payloads.Response.HealthStatus].value

      decoded shouldBe responseHealthStatus
    }
  }
