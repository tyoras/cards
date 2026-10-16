package io.tyoras.cards.domain.game.schnapsen

import io.circe.derivation.{ConfiguredCodec, ConfiguredEncoder, renaming}
import io.circe.{Codec, Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.tyoras.cards.domain.game.schnapsen.model.*
import io.tyoras.cards.domain.card.codecs.given
import io.tyoras.cards.domain.user.model.User
import pl.iterators.kebs.circe.KebsCirceSnakified

object codecs extends KebsCirceSnakified:

  given Codec[PlayerId] = Codec.implied[User.ID]

  given KeyEncoder[PlayerId] = KeyEncoder.encodeKeyUUID.contramap(_.unwrap)
  given KeyDecoder[PlayerId] = KeyDecoder.decodeKeyUUID.map(User.ID(_))

  given Codec[Marriage.Status] = ConfiguredCodec.derived
  given Codec[Marriage]        = ConfiguredCodec.derived
  given Codec[Player]          = ConfiguredCodec.derived
  given Codec[GameRound]       = ConfiguredCodec.derived
  given Codec[RoundOutcome]    = ConfiguredCodec.derived

  given ConfiguredCodec[GameState]      = ConfiguredCodec.derive(discriminator = Some("code"), transformMemberNames = renaming.snakeCase)
  given ConfiguredCodec[SchnapsenInput] = ConfiguredCodec.derive(discriminator = Some("input_type"), transformMemberNames = renaming.snakeCase)
