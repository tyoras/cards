package io.tyoras.cards.domain.game

abstract class GameError(val code: String, msg: String) extends Exception(msg)
