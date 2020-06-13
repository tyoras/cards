package io.tyoras.cards.cli.game.schnapsen

import io.tyoras.cards.GameError

sealed abstract class SchnapsenCliError(code: String, msg: String) extends GameError(code, msg)

case object InvalidInput extends SchnapsenCliError("invalid_input", "The input is not valid according to the current game state.")

case object InvalidState extends SchnapsenCliError("invalid_state", "The current state can not be handled.")
