package io.tyoras.cards.server.endpoints.utils

import cats.data.{Kleisli, OptionT}
import cats.effect.IO
import cats.syntax.all.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.server.endpoints.{Endpoint, ErrorHandling}
import org.http4s.server.Router
import org.http4s.{AuthedRequest, HttpRoutes, Request}

object TestRoutes:
  /** Public and authed routes of the endpoint exposed under `/api`, every authed request being authenticated as the given user. Errors are turned into
    * responses the same way the real server does.
    */
  def of(endpoint: Endpoint[IO], authenticatedAs: User.Existing): HttpRoutes[IO] =
    val authed = Kleisli((req: Request[IO]) => endpoint.authedRoutes.run(AuthedRequest(authenticatedAs, req)))
    val all    = endpoint.routes <+> authed
    Router("api" -> Kleisli((req: Request[IO]) => all.run(req).recoverWith { case t => OptionT.liftF(ErrorHandling.defaultErrorHandler[IO](t)) }))
