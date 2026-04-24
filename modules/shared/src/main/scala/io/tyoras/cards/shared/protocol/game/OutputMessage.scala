package io.tyoras.cards.shared.protocol.game

import cats.implicits.catsSyntaxOptionId
import io.chrisdavenport.fuuid.FUUID
import io.circe.{Decoder, Encoder, Json}
import io.circe.derivation.{ConfiguredCodec, renaming}
import io.chrisdavenport.fuuid.circe.given
import io.tyoras.cards.domain.game.{GameTyp, GameType}

enum OutputMessage:
  case KeepAlive
  case DiscardMessage
  case AuthError(code: String, msg: String)
  case UnsupportedCommand
  case PlayerConnectionSuccess(gameId: FUUID, gameType: GameType, playerId: FUUID, playerName: String)
  case PlayerDisconnected(gameId: FUUID, playerId: FUUID, playerName: String)
  case GameState(gameId: FUUID, recipient: FUUID, state: Json)
  case GameError(gameId: FUUID, recipient: FUUID, code: String, msg: String)
  case ProtocolError(gameId: FUUID, recipient: FUUID, code: String, msg: String)
  case GameEnded(gameId: FUUID)

object OutputMessage:
  given ConfiguredCodec[OutputMessage] = ConfiguredCodec.derive(discriminator = "message_type".some, transformMemberNames = renaming.snakeCase)
