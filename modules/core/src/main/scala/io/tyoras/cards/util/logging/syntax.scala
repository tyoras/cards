package io.tyoras.cards.util.logging

import cats.Show
import cats.syntax.all.*

object syntax:
  opaque type LogContextKey = String
  val playerIdKey: LogContextKey = "player_id"

  extension [A : Show](a: A)
    def ctx(key: LogContextKey): Map[String, String] = Map(key -> a.show)
    def ctxKey(key: LogContextKey): (String, String) = key -> a.show
