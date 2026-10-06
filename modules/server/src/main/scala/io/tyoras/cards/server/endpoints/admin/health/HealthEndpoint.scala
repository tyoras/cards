package io.tyoras.cards.server.endpoints.admin.health

import cats.effect.Sync
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.domain.healthcheck.{HealthStatus, Status}
import org.http4s.{HttpRoutes, Response}
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import cats.syntax.all.*
import io.tyoras.cards.server.healtcheck.ServerHealthMonitor
import io.tyoras.cards.shared.endpoint.admin.health.Payloads
import io.tyoras.cards.shared.endpoint.admin.health.Payloads.Response.HealthStatus.given
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.circe.*
import io.scalaland.chimney.dsl.transformInto

object HealthEndpoint:
  def of[F[_] : Sync](healthMonitor: ServerHealthMonitor[F]): F[Endpoint[F]] = Sync[F].delay {
    new Endpoint[F] with Http4sDsl[F] {

      override val routes: HttpRoutes[F] = Router {
        "health" -> HttpRoutes.of {
          case GET -> Root / "startup" => startup
          case GET -> Root / "ready"   => readiness
          case GET -> Root / "live"    => liveness
        }
      }

      private def startup: F[Response[F]] =
        healthMonitor.healthMonitor.startup.flatMap(response)

      private def readiness: F[Response[F]] =
        healthMonitor.healthMonitor.readiness.flatMap(response)

      private def liveness: F[Response[F]] =
        healthMonitor.healthMonitor.liveness.flatMap(response)

      private def response(healthStatus: HealthStatus): F[Response[F]] =
        val payload = healthStatus.transformInto[Payloads.Response.HealthStatus]
        healthStatus.status match
          case Status.DOWN => ServiceUnavailable(payload)
          case _           => Ok(payload)
    }
  }
