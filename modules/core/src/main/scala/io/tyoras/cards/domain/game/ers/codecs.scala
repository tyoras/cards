package io.tyoras.cards.domain.game.ers

import io.circe.{Codec, Decoder, DecodingFailure, Encoder, KeyDecoder, KeyEncoder}
import io.circe.derivation.{ConfiguredCodec, ConfiguredDecoder, ConfiguredEncoder, renaming}
import io.tyoras.cards.domain.card.codecs.given
import io.github.iltotore.iron.circe.given
import io.tyoras.cards.domain.game.ers.model.{ERSInput, Elimination, GameContext, ERsGameState, Player, PlayerGameState, PlayerId}
import io.tyoras.cards.domain.game.ers.model.ERsGameState.{ChallengeTurn, PlayerTurn}
import io.tyoras.cards.domain.user.model.User
import pl.iterators.kebs.circe.snakified.*

object codecs:
  given Codec[Player]      = Codec.implied
  given Codec[Elimination] = Codec.implied

  given KeyEncoder[PlayerId] = KeyEncoder.encodeKeyUUID.contramap(_.unwrap)
  given KeyDecoder[PlayerId] = KeyDecoder.decodeKeyUUID.map(User.ID(_))
  given Codec[GameContext]   = Codec.implied

  given Codec[PlayerTurn | ChallengeTurn] = Codec.from(
    Decoder.instance { c =>
      gameStateCodec(c).flatMap {
        case s: PlayerTurn    => Right(s)
        case s: ChallengeTurn => Right(s)
        case other            => Left(DecodingFailure(s"Expected player-turn or challenge-turn, got ${other.code}", c.history))
      }
    },
    Encoder.instance(s => gameStateCodec(s))
  )

  given gameStateCodec: ConfiguredCodec[ERsGameState] = ConfiguredCodec.derive(discriminator = Some("code"), transformMemberNames = renaming.snakeCase)
  given ConfiguredCodec[ERSInput]                     = ConfiguredCodec.derive(discriminator = Some("input_type"), transformMemberNames = renaming.snakeCase)
  given ConfiguredCodec[PlayerGameState]              = ConfiguredCodec.derive(discriminator = Some("code"), transformMemberNames = renaming.snakeCase)
