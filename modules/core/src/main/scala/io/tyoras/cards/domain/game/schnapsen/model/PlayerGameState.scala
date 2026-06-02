package io.tyoras.cards.domain.game.schnapsen.model

import io.circe.Codec
import io.circe.derivation.{ConfiguredCodec, renaming}
import io.tyoras.cards.domain.game.GameStateFilter
import io.tyoras.cards.domain.game.schnapsen.PlayerId

//TODO: implement player game state with all the information needed for a player to play (hand, opponents, etc.)
case class PlayerGameState()
object PlayerGameState:

  given ConfiguredCodec[PlayerGameState] = ConfiguredCodec.derive(discriminator = Some("code"), transformMemberNames = renaming.snakeCase)
  given GameStateFilter[GameState] with
    type PlayerState = PlayerGameState
    override def codec: Codec[PlayerGameState]                                                = summon[ConfiguredCodec[PlayerGameState]]
    extension (gameState: GameState) def filterForPlayer(playerId: PlayerId): PlayerGameState = PlayerGameState()
