package io.tyoras.cards.shared.protocol.game

import io.chrisdavenport.fuuid.FUUID
import io.circe.{Codec, Json}
import io.circe.derivation.{Configuration, ConfiguredCodec}
import io.tyoras.cards.domain.game.GameType
import io.chrisdavenport.fuuid.circe.given
import io.tyoras.cards.domain.game.GameTyp.given
import io.tyoras.cards.util.codecs.json.given

object Commands:
  final case class AuthCommand(gameId: FUUID, gameType: GameType, jwt: String) derives ConfiguredCodec

  enum AuthenticatedCommand:
    def gameId: FUUID
    case QuitCommand(gameId: FUUID)
    case StateCommand(gameId: FUUID)
    case GameCommand(gameId: FUUID, input: Json)

  object AuthenticatedCommand:
    given Configuration = io.tyoras.cards.util.codecs.json.given_Configuration.withDiscriminator("command_type")

    given Codec[AuthenticatedCommand] = Codec.AsObject.derivedConfigured
