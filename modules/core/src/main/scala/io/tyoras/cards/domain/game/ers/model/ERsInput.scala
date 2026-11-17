package io.tyoras.cards.domain.game.ers.model

import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.game.GameInput

sealed trait ERsInput extends GameInput:
  override val toString: String = label

object ERsInput:
  enum MetaInput(override val label: String) extends ERsInput:
    case Restart(override val playerId: PlayerId) extends MetaInput(s"Restart game ($playerId)")
    case End(override val playerId: PlayerId)     extends MetaInput(s"Quit game ($playerId)")

  enum GameInput(override val label: String) extends ERsInput:
    case Ready(override val playerId: PlayerId)                    extends GameInput(s"Ready to start next turn ($playerId)")
    case PlayCard(override val playerId: PlayerId)                 extends GameInput(s"Play next card ($playerId)")
    case Discard(override val playerId: PlayerId, cardId: Card.ID) extends GameInput(s"Discard card $cardId ($playerId)")
    case Slap(override val playerId: PlayerId)                     extends GameInput(s"Slap heap ($playerId)")
