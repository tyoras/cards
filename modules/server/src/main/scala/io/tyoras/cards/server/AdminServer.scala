package io.tyoras.cards.server

import cats.effect.{Async, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.Network
import io.tyoras.cards.server.config.HttpConfig
import io.tyoras.cards.server.endpoints.{Endpoint, ErrorHandling}
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Router

trait AdminServer[F[_]]:
  def serve: Resource[F, Unit]

object AdminServer:
  def of[F[_] : Async : Network](config: HttpConfig, adminHttpApp: HttpApp[F]): Server[F] = new Server[F]:
    override val serve: Resource[F, Unit] =
      EmberServerBuilder
        .default[F]
        .withHostOption(Host.fromString(config.host))
        .withPort(Port.fromInt(config.adminPort).getOrElse(Port.Wildcard))
        .withHttpApp(adminHttpApp)
        .withErrorHandler(ErrorHandling.defaultErrorHandler)
        .build
        .void

  object AdminHttpApp:
    def of[F[_] : Async](healthEndpoint: Endpoint[F]): HttpApp[F] =
      Router(
        "admin" -> healthEndpoint.routes
      ).orNotFound
