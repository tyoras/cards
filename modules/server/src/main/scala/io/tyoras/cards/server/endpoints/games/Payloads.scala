package io.tyoras.cards.server.endpoints.games

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

import io.scalaland.chimney.Transformer

object Payloads:
  given Encoder[GameType] = Encoder.encodeString.contramap(_.toString.toLowerCase)

  object Request:
    final case class Creation(players: List[FUUID])
    object Creation:
      given Decoder[Creation] = deriveDecoder

  object Response:
    final case class Game(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, gameType: GameType, players: NonEmptyList[FUUID])
    object Game:
      given Encoder[Game] = deriveEncoder

      given fromExisting: Transformer[Existing[?], Response.Game] =
        Transformer.define[Existing[?], Response.Game].enableMethodAccessors.buildTransformer

      // FIXME keeping this one because chimney does not find the implicit when the From type has a generic wildcard
      def fromExistingGame(game: Existing[_]): Response.Game =
        Response.Game(game.id, game.createdAt, game.updatedAt, game.gameType, game.players)
