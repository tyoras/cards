package io.tyoras.cards.domain.game.schnapsen

import io.circe.derivation.{ConfiguredCodec, renaming}
import io.circe.{Decoder, Encoder}
import io.tyoras.cards.domain.game.schnapsen.model.*
import io.chrisdavenport.fuuid.circe.given
import io.tyoras.cards.domain.card.codecs.given

object codecs:
  given Encoder[PlayerInfo]      = Encoder.derived
  given Encoder[Player]          = Encoder.derived
  given Encoder[GameContext]     = Encoder.derived
  given Encoder[Marriage.Status] = Encoder.derived
  given Encoder[Marriage]        = Encoder.derived
  given Encoder[RoundOutcome]    = Encoder.derived
  given Encoder[TalonClosing]    = Encoder.derived
  given Encoder[GameRound]       = Encoder.derived

  given Decoder[PlayerInfo]      = Decoder.derived
  given Decoder[Player]          = Decoder.derived
  given Decoder[GameContext]     = Decoder.derived
  given Decoder[Marriage.Status] = Decoder.derived
  given Decoder[Marriage]        = Decoder.derived
  given Decoder[RoundOutcome]    = Decoder.derived
  given Decoder[TalonClosing]    = Decoder.derived
  given Decoder[GameRound]       = Decoder.derived

  given ConfiguredCodec[GameState]      = ConfiguredCodec.derive(discriminator = Some("code"), transformMemberNames = renaming.snakeCase)
  given ConfiguredCodec[SchnapsenInput] = ConfiguredCodec.derive(discriminator = Some("input_type"), transformMemberNames = renaming.snakeCase)
