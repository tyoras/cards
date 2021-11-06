package io.tyoras.cards.server.endpoints

import org.http4s.HttpRoutes

trait Endpoint[F[_]]:
  def routes: HttpRoutes[F]
