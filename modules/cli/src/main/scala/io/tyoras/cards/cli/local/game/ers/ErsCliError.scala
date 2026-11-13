package io.tyoras.cards.cli.local.game.ers

import io.tyoras.cards.domain.game.GameError

enum ErsCliError(code: String, msg: String) extends GameError(code, msg):
  case InvalidSettings(msg: String) extends ErsCliError("invalid_settings", msg)
  case InvalidInput                 extends ErsCliError("invalid_input", "The input is not valid according to the current game state.")
  case InvalidState                 extends ErsCliError("invalid_state", "The current state can not be handled.")
  case UnexpectedError(msg: String) extends ErsCliError("unexpected_error", msg)
