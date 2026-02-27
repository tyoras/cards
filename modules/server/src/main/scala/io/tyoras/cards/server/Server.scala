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
import dev.profunktor.auth.JwtAuthMiddleware
import dev.profunktor.auth.jwt.JwtAuth
import fs2.io.net.Network
import io.tyoras.cards.domain.auth.{AuthConfig, AuthService}
import io.tyoras.cards.domain.user.User
import io.tyoras.cards.server.config.HttpConfig
import org.http4s.server.websocket.WebSocketBuilder2

import scala.util.chaining.*

trait Server[F[_]]:
  def serve: Resource[F, Unit]

object Server:
  def of[F[_] : Async : Network](config: HttpConfig, httpWSApp: HttpWsApp[F]): Server[F] = new Server[F] {
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

  type HttpWsApp[F[_]] = WebSocketBuilder2[F] => HttpApp[F]
  object HttpWsApp:
    def of[F[_] : Async](
        authConfig: AuthConfig,
        authService: AuthService[F]
    )(authEndpoint: Endpoint[F], first: Endpoint[F], remaining: Endpoint[F]*): HttpWsApp[F] = wsBuilder =>
      val jwtAuth        = JwtAuth.hmac(authConfig.secretKey.toCharArray, authConfig.hmacAlgo)
      val authMiddleware = JwtAuthMiddleware[F, User.Existing](jwtAuth, authService.authenticate)
      val all            = first +: remaining
      val authedRoutes   = all.map(_.authedRoutes).reduceLeft(_ <+> _).pipe(authMiddleware)
      val httpRoutes     = all.map(_.routes).reduceLeft(_ <+> _) <+> authedRoutes
      val wsRoutes       = all.map(_.wsRoutes(wsBuilder)).reduceLeft(_ <+> _)
      Router(
        "api"  -> httpRoutes,
        "auth" -> authEndpoint.routes,
        "ws"   -> wsRoutes
      ).orNotFound.pipe(Logger.httpApp(logHeaders = true, logBody = true))
