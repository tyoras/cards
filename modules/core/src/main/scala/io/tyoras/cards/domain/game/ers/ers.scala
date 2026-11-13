package io.tyoras.cards.domain.game.ers

import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.{Card, Rank, international52Deck}
import io.tyoras.cards.domain.game.ers.model.{GameState, PlayerId}

val ersDeck = international52Deck

val headCardChallenge: Map[Rank, Int] = Map(
  Ace()   -> 4,
  King()  -> 3,
  Queen() -> 2,
  Jack()  -> 1
)

// number of cards that the player getting the penalty has to discard
val penalty: Int = 1

extension (s: GameState)
  def pickFirstCard(playerId: PlayerId): Option[Card] =
    s.context.pickFirstCard(playerId)
