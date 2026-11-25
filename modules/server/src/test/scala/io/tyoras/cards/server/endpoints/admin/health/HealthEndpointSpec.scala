package io.tyoras.cards.server.endpoints.admin.health

import cats.effect.IO
import cats.effect.std.AtomicCell
import io.tyoras.cards.domain.healthcheck.{HealthMonitorService, Probe, Status as HealthStatus}
import io.tyoras.cards.server.endpoints.EndpointSpec
import io.tyoras.cards.server.healtcheck.ServerHealthMonitor
import io.tyoras.cards.shared.endpoint.admin.health.Payloads
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.server.Router
import org.http4s.{HttpRoutes, Status}
import pl.iterators.baklava.{FreeFormSchema, Schema, SchemaType}
import sttp.model.Method.GET

class HealthEndpointSpec extends EndpointSpec:

  /** Baklava cannot derive a schema for a recursive type: the probe status is described with its component statuses, whose own components (not produced by the
    * server probes) are described as free-form objects.
    */
  given Schema[Payloads.Response.HealthStatus] =
    def schema[T](
        name: String,
        tpe: SchemaType,
        props: Map[String, Schema[?]] = Map.empty,
        itemsSchema: Option[Schema[?]] = None,
        values: Option[Set[String]] = None,
        desc: Option[String] = None
    ): Schema[T] =
      new Schema[T]:
        val className: String                  = name
        val `type`: SchemaType                 = tpe
        val format: Option[String]             = None
        val properties: Map[String, Schema[?]] = props
        val items: Option[Schema[?]]           = itemsSchema
        val `enum`: Option[Set[String]]        = values
        val required: Boolean                  = true
        val additionalProperties: Boolean      = false
        val default: Option[T]                 = None
        val description: Option[String]        = desc

    val name   = schema[String]("String", SchemaType.StringType, desc = Some("Name of the checked component"))
    val status = schema[String]("HealthStatusValue", SchemaType.StringType, values = Some(Set("UP", "DEGRADED", "DOWN")))
    def healthStatus(className: String, components: Schema[?]) =
      schema[Payloads.Response.HealthStatus](
        className,
        SchemaType.ObjectType,
        props = Map(
          "name"       -> name,
          "status"     -> status,
          "components" -> schema[List[Any]](s"List[${components.className}]", SchemaType.ArrayType, itemsSchema = Some(components))
        )
      )
    healthStatus("HealthStatus", healthStatus("ComponentHealthStatus", FreeFormSchema[Any]("NestedHealthStatus")))

  /** Manual probes standing for the server ones: db migration (startup), db (readiness) and server (liveness) */
  private case class Env(routes: HttpRoutes[IO], dbMigration: Probe.Manual[IO], db: Probe.Manual[IO], server: Probe.Manual[IO]):
    def set(probe: Probe.Manual[IO], status: HealthStatus): Unit =
      (status match
        case HealthStatus.UP       => probe.up
        case HealthStatus.DEGRADED => probe.degraded
        case HealthStatus.DOWN     => probe.down
      ).unsafeRunSync()

  private def probe(name: String): IO[Probe.Manual[IO]] = AtomicCell[IO].of(HealthStatus.UP).map(Probe.Manual(_, name))

  /** Everything is up by default */
  private val makeEnv: IO[Env] =
    for
      dbMigrationProbe <- probe("db_migration")
      dbProbe          <- probe("db")
      serverProbe      <- probe("server")
      monitor <- HealthMonitorService.of[IO](startupProbes = List(dbMigrationProbe), readinessProbes = List(dbProbe), livenessProbes = List(serverProbe))
      serverMonitor = new ServerHealthMonitor[IO]:
        override def healthMonitor: HealthMonitorService[IO]    = monitor
        override def serverLivenessProbe: Probe.Manual[IO]      = serverProbe
        override def dbMigrationProgressProbe: Probe.Manual[IO] = dbMigrationProbe
      endpoint <- HealthEndpoint.of[IO](serverMonitor)
    yield Env(Router("admin" -> endpoint.routes), dbMigrationProbe, dbProbe, serverProbe)

  /** Run a test against a fresh environment */
  private def withEnv[A](test: Env => A): A = test(makeEnv.unsafeRunSync())

  override lazy val testedRoutes: HttpRoutes[IO] = makeEnv.unsafeRunSync().routes

  private def composite(name: String, status: String, component: String): Payloads.Response.HealthStatus =
    Payloads.Response.HealthStatus(name, status, List(Payloads.Response.HealthStatus(component, status, Nil)))

  path(path = "/admin/health/startup")(
    supports(
      GET,
      description = "Startup probe: UP once the server has finished starting (e.g. the database migration is done), then stays UP",
      summary = "Startup probe",
      tags = Seq("admin")
    )(
      onRequest.respondsWith[Payloads.Response.HealthStatus](Status.Ok, description = "The server has started").assert { ctx =>
        withEnv { e =>
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.Ok.code
          response.body shouldBe composite("startup", "UP", "db_migration")
        }
      },
      onRequest.respondsWith[Payloads.Response.HealthStatus](Status.ServiceUnavailable, description = "The server is still starting").assert { ctx =>
        withEnv { e =>
          e.set(e.dbMigration, HealthStatus.DOWN)
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.ServiceUnavailable.code
          response.body shouldBe composite("startup", "DOWN", "db_migration")
        }
      }
    )
  )

  path(path = "/admin/health/ready")(
    supports(
      GET,
      description = "Readiness probe: whether the server can handle requests (e.g. the database is reachable). DOWN while the server is starting",
      summary = "Readiness probe",
      tags = Seq("admin")
    )(
      onRequest.respondsWith[Payloads.Response.HealthStatus](Status.Ok, description = "The server is ready").assert { ctx =>
        withEnv { e =>
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.Ok.code
          response.body shouldBe composite("readiness", "UP", "db")
        }
      },
      onRequest.respondsWith[Payloads.Response.HealthStatus](Status.Ok, description = "The server is ready but in a degraded state").assert { ctx =>
        withEnv { e =>
          e.set(e.db, HealthStatus.DEGRADED)
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.Ok.code
          response.body shouldBe composite("readiness", "DEGRADED", "db")
        }
      },
      onRequest.respondsWith[Payloads.Response.HealthStatus](Status.ServiceUnavailable, description = "The server is not ready").assert { ctx =>
        withEnv { e =>
          e.set(e.db, HealthStatus.DOWN)
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.ServiceUnavailable.code
          response.body shouldBe composite("readiness", "DOWN", "db")
        }
      }
    )
  )

  path(path = "/admin/health/live")(
    supports(
      GET,
      description = "Liveness probe: whether the server is alive. DOWN while the server is starting",
      summary = "Liveness probe",
      tags = Seq("admin")
    )(
      onRequest.respondsWith[Payloads.Response.HealthStatus](Status.Ok, description = "The server is alive").assert { ctx =>
        withEnv { e =>
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.Ok.code
          response.body shouldBe composite("liveness", "UP", "server")
        }
      },
      onRequest.respondsWith[Payloads.Response.HealthStatus](Status.ServiceUnavailable, description = "The server is down").assert { ctx =>
        withEnv { e =>
          e.set(e.server, HealthStatus.DOWN)
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.ServiceUnavailable.code
          response.body shouldBe composite("liveness", "DOWN", "server")
        }
      },
      onRequest.respondsWith[Payloads.Response.HealthStatus](Status.ServiceUnavailable, description = "The server is still starting").assert { ctx =>
        withEnv { e =>
          e.set(e.dbMigration, HealthStatus.DOWN)
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.ServiceUnavailable.code
          response.body shouldBe Payloads.Response.HealthStatus("liveness", "DOWN", Nil)
        }
      }
    )
  )
