package io.tyoras.cards

abstract class GameError(val code: String, msg: String) extends Exception(msg)
