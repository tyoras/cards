package io.tyoras.cards.domain.game.war

import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.game.war.model.*

val warDeck = international52Deck

extension (s: GameState)
  def pickFirstCard(playerId: PlayerId): Option[Card] =
    s.context.pickFirstCard(playerId)
