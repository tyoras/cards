package io.tyoras.cards.server

import cats.effect.{Async, Resource}
import cats.syntax.all.*
import io.tyoras.cards.server.endpoints.{Endpoint, ErrorHandling, WsEndpoint}
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Router
import org.http4s.server.middleware.Logger
import com.comcast.ip4s.*
import fs2.io.net.Network
import io.tyoras.cards.server.config.HttpConfig
import org.http4s.server.websocket.WebSocketBuilder2

import scala.util.chaining.*

trait Server[F[_]]:
  def serve: Resource[F, Unit]

object Server:
  def of[F[_] : Async : Network](config: HttpConfig, httpWSApp: WebSocketBuilder2[F] => HttpApp[F]): Server[F] = new Server[F] {
    override val serve: Resource[F, Unit] =
      EmberServerBuilder
        .default[F]
        .withHostOption(Host.fromString(config.host))
        .withPort(Port.fromInt(config.port).getOrElse(Port.Wildcard))
        .withHttpWebSocketApp(httpWSApp)
        .withErrorHandler(ErrorHandling.defaultErrorHandler)
        .build
        .void
  }

  object HttpWsApp:
    def of[F[_] : Async](first: Endpoint[F], remaining: Endpoint[F]*): WebSocketBuilder2[F] => HttpApp[F] = wsBuilder =>
      val all        = first +: remaining
      val httpRoutes = all.map(_.routes).reduceLeft(_ <+> _)
      val wsRoutes   = all.collect { case e: WsEndpoint[F] => e.wsRoutes(wsBuilder) }.reduceLeftOption(_ <+> _).getOrElse(HttpRoutes.empty[F])
      Router("api" -> httpRoutes, "ws" -> wsRoutes).orNotFound.pipe(Logger.httpApp(logHeaders = true, logBody = true))
