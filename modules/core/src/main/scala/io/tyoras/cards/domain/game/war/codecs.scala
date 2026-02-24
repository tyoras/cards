package io.tyoras.cards.domain.game.war

import io.circe.{Decoder, Encoder}
import io.chrisdavenport.fuuid.circe.given
import io.tyoras.cards.domain.game.war.model.{Elimination, GameContext, Player}
import io.tyoras.cards.domain.card.given

given Encoder[Player]      = Encoder.derived
given Encoder[Elimination] = Encoder.derived
given Encoder[GameContext] = Encoder.derived

given Decoder[Player]      = Decoder.derived
given Decoder[Elimination] = Decoder.derived
given Decoder[GameContext] = Decoder.derived
