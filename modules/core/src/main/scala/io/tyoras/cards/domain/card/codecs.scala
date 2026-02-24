package io.tyoras.cards.domain.card

import io.circe.{Decoder, Encoder}

given Encoder[Rank] = Encoder.derived
given Encoder[Suit] = Encoder.derived
given Encoder[Card] = Encoder.derived

given Decoder[Rank] = Decoder.derived
given Decoder[Suit] = Decoder.derived
given Decoder[Card] = Decoder.derived
