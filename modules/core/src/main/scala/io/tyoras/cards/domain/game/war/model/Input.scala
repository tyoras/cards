package io.tyoras.cards.domain.game.war.model

import io.tyoras.cards.domain.card.Card

trait Input:
  def label: String
  def playerId: PlayerId

  override val toString: String = label

enum MetaInput(override val label: String) extends Input:
  case Restart(override val playerId: PlayerId) extends MetaInput(s"Restart game ($playerId)")
  case End(override val playerId: PlayerId)     extends MetaInput(s"Quit game ($playerId)")

enum GameInput(override val label: String) extends Input:
  case Ready(override val playerId: PlayerId)                extends GameInput(s"Ready to start next turn ($playerId)")
  case PlayCard(override val playerId: PlayerId, card: Card) extends GameInput(s"Play card $card ($playerId)")
