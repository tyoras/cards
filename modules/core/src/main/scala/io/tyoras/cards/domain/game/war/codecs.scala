package io.tyoras.cards.domain.game.war

import io.circe.{Codec, Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.circe.derivation.{ConfiguredCodec, ConfiguredDecoder, ConfiguredEncoder, renaming}
import io.tyoras.cards.domain.game.war.model.{Elimination, GameContext, GameState, Player, PlayerGameState, PlayerId, Turn, WarInput}
import io.tyoras.cards.domain.card.codecs.given
import io.tyoras.cards.domain.game.war.model.GameState.WarTurn.BattleRound
import io.github.iltotore.iron.circe.given
import io.tyoras.cards.domain.user.model.User
import pl.iterators.kebs.circe.snakified.*

object codecs:
  given Codec[Turn]        = Codec.implied
  given Codec[Player]      = Codec.implied
  given Codec[BattleRound] = Codec.implied

  given KeyEncoder[PlayerId]             = KeyEncoder.encodeKeyUUID.contramap(_.unwrap)
  given KeyDecoder[PlayerId]             = KeyDecoder.decodeKeyUUID.map(User.ID(_))
  given Codec[GameContext]               = Codec.implied
  given ConfiguredCodec[GameState]       = ConfiguredCodec.derive(discriminator = Some("code"), transformMemberNames = renaming.snakeCase)
  given ConfiguredCodec[WarInput]        = ConfiguredCodec.derive(discriminator = Some("input_type"), transformMemberNames = renaming.snakeCase)
  given ConfiguredCodec[PlayerGameState] = ConfiguredCodec.derive(discriminator = Some("code"), transformMemberNames = renaming.snakeCase)
