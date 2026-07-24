package io.tyoras.cards.persistence

import io.tyoras.cards.util.error.TechnicalError

case class PersistenceError(override val code: String, msg: String) extends TechnicalError(code, msg)

enum ParsingError(msg: String) extends TechnicalError("parsing_error", msg):
  case InvalidEnumValue(enumName: String, value: String) extends ParsingError(s"Invalid value '$value' for enum '$enumName'")
  case InvalidCombination(msg: String)                   extends ParsingError(msg)
