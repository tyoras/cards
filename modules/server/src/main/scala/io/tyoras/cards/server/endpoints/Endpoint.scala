package io.tyoras.cards.server.endpoints

import org.http4s.HttpRoutes
import org.http4s.server.websocket.WebSocketBuilder2

trait Endpoint[F[_]]:
  def routes: HttpRoutes[F]

trait WsEndpoint[F[_]] extends Endpoint[F]:
  def wsRoutes: WebSocketBuilder2[F] => HttpRoutes[F]
