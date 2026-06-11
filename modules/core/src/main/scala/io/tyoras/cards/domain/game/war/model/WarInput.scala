package io.tyoras.cards.domain.game.war.model

import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.game.GameInput

sealed trait WarInput extends GameInput:
  override val toString: String = label

object WarInput:
  enum MetaInput(override val label: String) extends WarInput:
    case Restart(override val playerId: PlayerId) extends MetaInput(s"Restart game ($playerId)")
    case End(override val playerId: PlayerId)     extends MetaInput(s"Quit game ($playerId)")

  enum GameInput(override val label: String) extends WarInput:
    case Ready(override val playerId: PlayerId)                     extends GameInput(s"Ready to start next turn ($playerId)")
    case PlayCard(override val playerId: PlayerId, cardId: Card.ID) extends GameInput(s"Play card $cardId ($playerId)")
