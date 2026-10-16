package io.tyoras.cards.shared.endpoint.games

import cats.data.NonEmptyList
import io.circe.derivation.ConfiguredCodec
import io.circe.{Codec, Decoder, Encoder}
import io.scalaland.chimney.Transformer
import io.tyoras.cards.domain.game.Game.Existing
import io.tyoras.cards.domain.game.Game.ID as GameID
import io.tyoras.cards.domain.game.GameType
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.util.codecs.json.given
import pl.iterators.kebs.circe.snakified.{flatDecoder, flatEncoder}

import java.time.ZonedDateTime
import java.util.UUID

object Payloads:
  object Request:
    final case class Creation(players: List[UUID]) derives Codec

  object Response:
    given Decoder[GameID]  = flatDecoder
    given Encoder[GameID]  = flatEncoder
    given Decoder[User.ID] = flatDecoder
    given Encoder[User.ID] = flatEncoder
    final case class Game(id: GameID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, gameType: GameType, players: NonEmptyList[User.ID])
        derives ConfiguredCodec
    object Game:
      given fromExisting: Transformer[Existing[?], Response.Game] = ???
//        Transformer.define[Existing[?], Response.Game].enableMethodAccessors.buildTransformer

      // FIXME keeping this one because chimney does not find the implicit when the From type has a generic wildcard
      def fromExistingGame(game: Existing[?]): Response.Game =
        Response.Game(game.id, game.createdAt, game.updatedAt, game.gameType, game.players)
