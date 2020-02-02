package io.tyoras.cards.cli.game.schnapsen

import io.tyoras.cards.GameError

sealed abstract class SchnapsenCliError(code: String, msg: String) extends GameError(code, msg)

case object InvalidInput extends SchnapsenCliError("invalid_input", "The input is not valid according to the current game state.")
