package io.tyoras.cards.domain.game.ers.model

import io.tyoras.cards.domain.game.GameError

sealed abstract class ERSError(code: String, msg: String) extends GameError(code, msg)

case class DeckError(msg: String) extends ERSError("deck_error", msg)

case object WrongPlayer extends ERSError("wrong_player", "Another player than the expected one has tried to play.")

case class InvalidAction(msg: String = "The player has tried to play an invalid action.") extends ERSError("invalid_action", msg)

case class InvalidCard(msg: String = "The player has tried to play an invalid card.") extends ERSError("invalid_card", msg)

case class InvalidPlayer(msg: String) extends ERSError("invalid_player", msg)

case class InvalidState(msg: String) extends ERSError("invalid_state", msg)
