package io.tyoras.cards.domain.game.schnapsen.model

import io.tyoras.cards.domain.game.GameError

sealed abstract class SchnapsenError(code: String, msg: String) extends GameError(code, msg)

case class DeckError(msg: String) extends SchnapsenError("deck_error", msg)

case object WrongPlayer extends SchnapsenError("wrong_player", "Another player than the expected one has tried to play.")

case class InvalidAction(msg: String = "The player has tried to play an invalid action.") extends SchnapsenError("invalid_action", msg)

case class InvalidCard(msg: String = "The player has tried to play an invalid card.") extends SchnapsenError("invalid_card", msg)

case class InvalidPlayer(msg: String) extends SchnapsenError("invalid_player", msg)
