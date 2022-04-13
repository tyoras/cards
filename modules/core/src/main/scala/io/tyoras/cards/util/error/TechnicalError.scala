package io.tyoras.cards.util.error

abstract class TechnicalError(val code: String, msg: String) extends Exception(msg)
