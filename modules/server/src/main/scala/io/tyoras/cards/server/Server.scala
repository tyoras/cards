package io.tyoras.cards.server

import cats.effect.{Async, Resource}
import cats.syntax.all._
import io.tyoras.cards.config.ServerConfig
import io.tyoras.cards.server.endpoints.Endpoint
import org.http4s.HttpApp
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware.Logger

import scala.concurrent.ExecutionContext
import scala.util.chaining._

trait Server[F[_]] {
  def serve: Resource[F, Unit]
}
object Server {
  def of[F[_] : Async](ec: ExecutionContext)(config: ServerConfig, httpApp: HttpApp[F]): Server[F] = new Server[F] {
    override val serve: Resource[F, Unit] = BlazeServerBuilder[F](ec).bindHttp(config.port, config.host).withHttpApp(httpApp).resource.void
  }

  object HttpApp {
    def of[F[_] : Async](first: Endpoint[F], remaining: Endpoint[F]*): HttpApp[F] =
      (first +: remaining)
        .map(_.routes)
        .reduceLeft(_ <+> _)
        .pipe(routes => Router("api" -> routes))
        .orNotFound
        .pipe(Logger.httpApp(logHeaders = true, logBody = true))
  }
}
