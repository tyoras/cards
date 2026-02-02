package io.tyoras.cards.server

import cats.effect.{Async, Resource}
import cats.syntax.all.*
import io.tyoras.cards.server.endpoints.{Endpoint, ErrorHandling}
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Router
import org.http4s.server.middleware.Logger
import com.comcast.ip4s.*
import fs2.io.net.Network
import io.tyoras.cards.server.config.HttpConfig

import scala.util.chaining.*

trait Server[F[_]]:
  def serve: Resource[F, Unit]

object Server:
  def of[F[_] : Async : Network](config: HttpConfig, httpApp: HttpApp[F]): Server[F] = new Server[F] {
    override val serve: Resource[F, Unit] =
      EmberServerBuilder
        .default[F]
        .withHostOption(Host.fromString(config.host))
        .withPort(Port.fromInt(config.port).getOrElse(Port.Wildcard))
        .withHttpApp(httpApp)
        .withErrorHandler(ErrorHandling.defaultErrorHandler)
        .build
        .void
  }

  object HttpApp:
    def of[F[_] : Async](first: Endpoint[F], remaining: Endpoint[F]*): HttpApp[F] =
      (first +: remaining)
        .map(_.routes)
        .reduceLeft(_ <+> _)
        .pipe(routes => Router("api" -> routes))
        .orNotFound
        .pipe(Logger.httpApp(logHeaders = true, logBody = true))
