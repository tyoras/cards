package io.tyoras.cards.shared.endpoint.games

import cats.data.NonEmptyList
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe.*
import io.circe.derivation.{Configuration, ConfiguredCodec}
import io.circe.{Codec, Decoder, Encoder}
import io.scalaland.chimney.Transformer
import io.tyoras.cards.domain.game.Game.Existing
import io.tyoras.cards.domain.game.GameType

import java.time.ZonedDateTime

object Payloads:
  given Configuration = Configuration.default.withSnakeCaseMemberNames
  object Request:
    final case class Creation(players: List[FUUID]) derives Codec

  object Response:
    final case class Game(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, gameType: GameType[?, ?], players: NonEmptyList[FUUID])
        derives ConfiguredCodec
    object Game:
      given fromExisting: Transformer[Existing[?], Response.Game] = ???
//        Transformer.define[Existing[?], Response.Game].enableMethodAccessors.buildTransformer

      // FIXME keeping this one because chimney does not find the implicit when the From type has a generic wildcard
      def fromExistingGame(game: Existing[?]): Response.Game =
        Response.Game(game.id, game.createdAt, game.updatedAt, game.gameType, game.players)
