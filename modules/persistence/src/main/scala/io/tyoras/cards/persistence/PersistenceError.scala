package io.tyoras.cards.persistence

import io.tyoras.cards.util.error.TechnicalError

case class PersistenceError(override val code: String, msg: String) extends TechnicalError(code, msg)
