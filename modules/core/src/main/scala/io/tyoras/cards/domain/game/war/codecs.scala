package io.tyoras.cards.domain.game.war

import io.circe.{Decoder, Encoder}
import io.chrisdavenport.fuuid.circe.given
import io.circe.derivation.{Configuration, ConfiguredCodec, ConfiguredDecoder, ConfiguredEncoder, renaming}
import io.tyoras.cards.domain.game.war.model.{Elimination, GameContext, GameState, Player, WarInput}
import io.tyoras.cards.domain.card.given
import io.tyoras.cards.domain.game.war.model.GameState.WarTurn.BattleRound

object codecs:
  given Configuration = Configuration.default.withSnakeCaseMemberNames

  given ConfiguredCodec[Player]      = ConfiguredCodec.derived
  given ConfiguredCodec[Elimination] = ConfiguredCodec.derived
  given ConfiguredCodec[GameContext] = ConfiguredCodec.derived
  given ConfiguredCodec[BattleRound] = ConfiguredCodec.derived

  given ConfiguredCodec[GameState] = ConfiguredCodec.derive(discriminator = Some("code"), transformMemberNames = renaming.snakeCase)
  given ConfiguredCodec[WarInput]  = ConfiguredCodec.derive(discriminator = Some("input_type"), transformMemberNames = renaming.snakeCase)
