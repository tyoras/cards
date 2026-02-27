package io.tyoras.cards.server.endpoints

import cats.Applicative
import io.tyoras.cards.domain.user.User
import org.http4s.{AuthedRoutes, HttpRoutes}
import org.http4s.server.websocket.WebSocketBuilder2

abstract class Endpoint[F[_] : Applicative]:
  def routes: HttpRoutes[F]                           = HttpRoutes.empty
  def authedRoutes: AuthedRoutes[User.Existing, F]    = AuthedRoutes.empty
  def wsRoutes: WebSocketBuilder2[F] => HttpRoutes[F] = _ => HttpRoutes.empty
