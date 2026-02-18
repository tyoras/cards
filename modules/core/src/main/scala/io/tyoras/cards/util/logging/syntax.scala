package io.tyoras.cards.util.logging.syntax

import cats.Show
import cats.syntax.all.*

opaque type LogContextKey = String
val playerIdKey: LogContextKey = "player_id"

extension [A : Show](a: A)
  def ctx(key: LogContextKey): Map[String, String] = Map(key -> a.show)
  def ctxKey(key: LogContextKey): (String, String) = key -> a.show
