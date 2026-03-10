package io.tyoras.cards.domain.game.schnapsen.model

import io.tyoras.cards.domain.game.schnapsen.PlayerId
import io.tyoras.cards.domain.card.{Card, Suit}
import io.tyoras.cards.domain.game.GameInput

sealed trait SchnapsenInput extends GameInput:
  override def toString = s"$label"

object SchnapsenInput:
  enum MetaInput(override val label: String) extends SchnapsenInput:
    case Start(override val playerId: PlayerId)   extends MetaInput("Start game")
    case Restart(override val playerId: PlayerId) extends MetaInput("Restart game")
    case End(override val playerId: PlayerId)     extends MetaInput("Quit game")

  enum GameInput(override val label: String) extends SchnapsenInput:
    case PlayCard(override val playerId: PlayerId, card: Card) extends GameInput(s"Play card $card")
    case ExchangeTrumpJack(override val playerId: PlayerId)    extends GameInput("Exchange trump jack")
    case CloseTalon(override val playerId: PlayerId)           extends GameInput("Close the talon")
    case Meld(override val playerId: PlayerId, suit: Suit)     extends GameInput(s"Meld $suit King and $suit Queen")
    case ClaimVictory(override val playerId: PlayerId)         extends GameInput("Claim victory")
