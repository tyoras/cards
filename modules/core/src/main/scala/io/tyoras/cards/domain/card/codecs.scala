package io.tyoras.cards.domain.card

import io.circe.derivation.{ConfiguredCodec, ConfiguredEnumCodec}
import io.circe.{Codec, Decoder, Encoder, Json}
import io.tyoras.cards.domain.card.{Card, Rank, Suit}
import io.tyoras.cards.util.codecs.json.given
import io.github.iltotore.iron.circe.given

object codecs:
  given Codec[Rank]         = Codec.derived
  given Codec[Suit]         = ConfiguredEnumCodec.derive()
  given Encoder[Card.ID]    = id => Json.fromString(id.value)
  given Decoder[Card.ID]    = Decoder.decodeString.emap(Card.ID.either)
  given Encoder[Card.Value] = cValue => Json.fromInt(cValue.value)
  given Decoder[Card.Value] = Decoder.decodeInt.emap(Card.Value.either)
  given Encoder[Card.Count] = summon
  given Decoder[Card.Count] = summon
  given Codec[Card]         = ConfiguredCodec.derived
