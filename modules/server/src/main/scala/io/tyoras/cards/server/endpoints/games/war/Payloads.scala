package io.tyoras.cards.server.endpoints.games.war

import cats.data.NonEmptyList
import cats.implicits.catsSyntaxTuple2Semigroupal
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.*
import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder}
import io.tyoras.cards.domain.game.Game.Existing
import io.tyoras.cards.domain.game.GameType
import io.tyoras.cards.util.validation.syntax.*

import java.time.ZonedDateTime

object Payloads:
  object Request:
    final case class Creation(player1: FUUID, player2: FUUID)
    object Creation:
      given Decoder[Creation] = deriveDecoder
