package io.tyoras.cards.cli.game.war

import io.tyoras.cards.domain.game.GameError

enum WarCliError(code: String, msg: String) extends GameError(code, msg):
  case InvalidSettings(msg: String) extends WarCliError("invalid_settings", msg)
  case InvalidInput                 extends WarCliError("invalid_input", "The input is not valid according to the current game state.")
  case InvalidState                 extends WarCliError("invalid_state", "The current state can not be handled.")
  case UnexpectedError(msg: String) extends WarCliError("unexpected_error", msg)
