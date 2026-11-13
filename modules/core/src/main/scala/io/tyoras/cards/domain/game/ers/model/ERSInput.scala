package io.tyoras.cards.domain.game.ers.model

import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.game.GameInput

sealed trait ERSInput extends GameInput:
  override val toString: String = label

object ERSInput:
  enum MetaInput(override val label: String) extends ERSInput:
    case Restart(override val playerId: PlayerId) extends MetaInput(s"Restart game ($playerId)")
    case End(override val playerId: PlayerId)     extends MetaInput(s"Quit game ($playerId)")

  enum GameInput(override val label: String) extends ERSInput:
    case Ready(override val playerId: PlayerId)                     extends GameInput(s"Ready to start next turn ($playerId)")
    case PlayCard(override val playerId: PlayerId, cardId: Card.ID) extends GameInput(s"Play card $cardId ($playerId)")
    case Slap(override val playerId: PlayerId)                      extends GameInput(s"Slap heap ($playerId)")
