package io.tyoras.cards.domain.game

import scala.util.control.NoStackTrace

abstract class GameError(val code: String, msg: String) extends Exception(msg) with NoStackTrace
