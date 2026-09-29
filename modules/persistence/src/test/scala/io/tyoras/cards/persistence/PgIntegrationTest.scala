package io.tyoras.cards.persistence

import cats.effect.testing.scalatest.AsyncIOSpec
import com.dimafeng.testcontainers.{ContainerDef, PostgreSQLContainer}
import com.dimafeng.testcontainers.scalatest.TestContainerForAll
import io.tyoras.cards.persistence.config.DatabaseConfig
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import cats.effect.{IO, Resource}
import org.typelevel.otel4s.metrics.Meter
import org.typelevel.otel4s.trace.Tracer
import skunk.Session

class PgIntegrationTest extends AsyncWordSpec with AsyncIOSpec with TestContainerForAll with Matchers:
  import PgIntegrationTest.given

  override val containerDef: ContainerDef = PostgreSQLContainer.Def()

  protected lazy val sessionPool: Resource[IO, Session[IO]] = withContainers { case container: PostgreSQLContainer =>
    SessionPool
      .of[IO](DatabaseConfig(container.host, container.mappedPort(5432), container.username, container.password, container.databaseName, 10))
      .allocated
      .unsafeRunSync()
      ._1
  }

object PgIntegrationTest:
  given Tracer[IO] = Tracer.noop[IO]
  given Meter[IO]  = Meter.noop[IO]
