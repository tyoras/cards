package io.tyoras.cards.domain.game.war

import io.circe.{Codec, Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.circe.derivation.{ConfiguredCodec, ConfiguredDecoder, ConfiguredEncoder, renaming}
import io.tyoras.cards.domain.game.war.model.{Elimination, GameContext, WarGameState, Player, PlayerGameState, PlayerId, WarInput}
import io.tyoras.cards.domain.card.codecs.given
import io.tyoras.cards.domain.game.war.model.WarGameState.WarTurn.BattleRound
import io.github.iltotore.iron.circe.given
import io.tyoras.cards.domain.user.model.User
import pl.iterators.kebs.circe.snakified.*

object codecs:
  given Codec[Player]      = Codec.implied
  given Codec[BattleRound] = Codec.implied
  given Codec[Elimination] = Codec.implied

  given KeyEncoder[PlayerId]             = KeyEncoder.encodeKeyUUID.contramap(_.unwrap)
  given KeyDecoder[PlayerId]             = KeyDecoder.decodeKeyUUID.map(User.ID(_))
  given Codec[GameContext]               = Codec.implied
  given ConfiguredCodec[WarGameState]    = ConfiguredCodec.derive(discriminator = Some("code"), transformMemberNames = renaming.snakeCase)
  given ConfiguredCodec[WarInput]        = ConfiguredCodec.derive(discriminator = Some("input_type"), transformMemberNames = renaming.snakeCase)
  given ConfiguredCodec[PlayerGameState] = ConfiguredCodec.derive(discriminator = Some("code"), transformMemberNames = renaming.snakeCase)
