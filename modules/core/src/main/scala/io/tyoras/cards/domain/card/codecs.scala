package io.tyoras.cards.domain.card

import io.circe.derivation.{ConfiguredEnumDecoder, ConfiguredEnumEncoder}
import io.circe.{Decoder, Encoder}

given Encoder[Rank] = Encoder.derived
given Encoder[Suit] = ConfiguredEnumEncoder.derive()
given Encoder[Card] = Encoder.derived

given Decoder[Rank] = Decoder.derived
given Decoder[Suit] = ConfiguredEnumDecoder.derive()
given Decoder[Card] = Decoder.derived
