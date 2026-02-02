package io.tyoras.cards.server.endpoints.games.war

import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.*
import io.circe.generic.semiauto.*
import io.circe.Decoder

object Payloads:
  object Request:
    final case class Creation(player1: FUUID, player2: FUUID)
    object Creation:
      given Decoder[Creation] = deriveDecoder
