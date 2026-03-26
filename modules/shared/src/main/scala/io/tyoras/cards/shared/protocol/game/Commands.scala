package io.tyoras.cards.shared.protocol.game

import io.chrisdavenport.fuuid.FUUID
import io.circe.Json
import io.circe.derivation.{Configuration, ConfiguredCodec}
import io.tyoras.cards.domain.game.GameType
import io.chrisdavenport.fuuid.circe.given
import io.tyoras.cards.domain.game.GameTyp.given

object Commands:
  given Configuration = Configuration.default.withSnakeCaseMemberNames
  final case class AuthCommand(gameId: FUUID, gameType: GameType, jwt: String) derives ConfiguredCodec

  final case class GameCommand(gameId: FUUID, gameType: GameType, input: Json) derives ConfiguredCodec
