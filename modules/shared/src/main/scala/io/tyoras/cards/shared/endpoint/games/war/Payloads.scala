package io.tyoras.cards.shared.endpoint.games.war

import cats.data.NonEmptyList
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.*
import io.circe.Codec

object Payloads:
  object Request:
    final case class Creation(players: NonEmptyList[FUUID]) derives Codec
