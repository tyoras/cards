package io.tyoras.cards.domain.game.war

import io.circe.{Decoder, Encoder}
import io.chrisdavenport.fuuid.circe.given
import io.circe.derivation.{ConfiguredDecoder, ConfiguredEncoder}
import io.tyoras.cards.domain.game.war.model.{Elimination, GameContext, GameState, Player}
import io.tyoras.cards.domain.card.given
import io.tyoras.cards.domain.game.war.model.GameState.WarTurn.BattleRound

given Encoder[Player]      = Encoder.derived
given Encoder[Elimination] = Encoder.derived
given Encoder[GameContext] = Encoder.derived
given Encoder[BattleRound] = Encoder.derived

given Decoder[Player]      = Decoder.derived
given Decoder[Elimination] = Decoder.derived
given Decoder[GameContext] = Decoder.derived
given Decoder[BattleRound] = Decoder.derived

given encoder: Encoder[GameState] = ConfiguredEncoder.derive(discriminator = Some("code"))
given decoder: Decoder[GameState] = ConfiguredDecoder.derive(discriminator = Some("code"))
