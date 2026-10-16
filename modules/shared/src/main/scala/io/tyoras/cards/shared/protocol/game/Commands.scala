package io.tyoras.cards.shared.protocol.game

import io.circe.{Codec, Json}
import io.circe.derivation.{Configuration, ConfiguredCodec}
import io.tyoras.cards.domain.game.{Game, GameType}
import io.tyoras.cards.domain.game.GameTyp.given
import pl.iterators.kebs.circe.KebsCirceSnakified

object Commands extends KebsCirceSnakified:
  final case class AuthCommand(gameId: Game.ID, gameType: GameType, jwt: String) derives ConfiguredCodec

  enum AuthenticatedCommand:
    def gameId: Game.ID
    case QuitCommand(gameId: Game.ID)
    case StateCommand(gameId: Game.ID)
    case GameCommand(gameId: Game.ID, input: Json)

  object AuthenticatedCommand:
    given Configuration = io.tyoras.cards.util.codecs.json.given_Configuration.withDiscriminator("command_type")

    given Codec[AuthenticatedCommand] = Codec.AsObject.derivedConfigured
