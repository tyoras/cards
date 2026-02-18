package io.tyoras.cards.util.collection

import cats.Order
import cats.data.NonEmptySet

import scala.collection.immutable.SortedSet

object syntax:
  extension [A : Order](set: Set[A])
    def toNes: NonEmptySet[A] =
      given Ordering[A] = Order[A].toOrdering
      NonEmptySet.fromSetUnsafe(SortedSet.from(set))

  extension [A : Order](list: List[A]) def toNes: NonEmptySet[A] = list.toSet.toNes
