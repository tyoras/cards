package io.tyoras.cards.cli.game.schnapsen

import io.tyoras.cards.domain.game.GameError

enum SchnapsenCliError(code: String, msg: String) extends GameError(code, msg):
  case InvalidInput extends SchnapsenCliError("invalid_input", "The input is not valid according to the current game state.")
  case InvalidState extends SchnapsenCliError("invalid_state", "The current state can not be handled.")
