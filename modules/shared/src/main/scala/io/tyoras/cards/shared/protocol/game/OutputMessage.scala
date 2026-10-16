package io.tyoras.cards.shared.protocol.game

import cats.implicits.catsSyntaxOptionId
import io.circe.{Decoder, Encoder, Json}
import io.circe.derivation.{ConfiguredCodec, renaming}
import io.tyoras.cards.domain.game.{Game, GameTyp, GameType}
import io.tyoras.cards.domain.user.model.User
import pl.iterators.kebs.circe.KebsCirceSnakified

enum OutputMessage:
  case KeepAlive
  case DiscardMessage
  case AuthError(code: String, msg: String)
  case UnsupportedCommand
  case PlayerConnectionSuccess(gameId: Game.ID, gameType: GameType, playerId: User.ID, playerName: String)
  case PlayerDisconnected(gameId: Game.ID, playerId: User.ID, playerName: String)
  case GameState(gameId: Game.ID, recipient: User.ID, state: Json)
  case GameError(gameId: Game.ID, recipient: User.ID, code: String, msg: String)
  case ProtocolError(gameId: Game.ID, recipient: User.ID, code: String, msg: String)
  case GameEnded(gameId: Game.ID)

object OutputMessage extends KebsCirceSnakified:
  given ConfiguredCodec[OutputMessage] = ConfiguredCodec.derive(discriminator = "message_type".some, transformMemberNames = renaming.snakeCase)
