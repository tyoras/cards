package io.tyoras.cards.tests.util

import io.tyoras.cards.util.collection.syntax.*
import io.tyoras.cards.util.logging.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SyntaxSpec extends AnyFlatSpec with Matchers:

  "Set.toNes" should "build a sorted non-empty set" in {
    Set(3, 1, 2).toNes.toSortedSet.toList shouldBe List(1, 2, 3)
  }

  "List.toNes" should "build a sorted non-empty set without duplicates" in {
    List(2, 1, 2).toNes.toSortedSet.toList shouldBe List(1, 2)
  }

  "ctx" should "build a logging context" in {
    42.ctx(playerIdKey) shouldBe Map("player_id" -> "42")
  }

  "ctxKey" should "build a logging context entry" in {
    42.ctxKey(playerIdKey) shouldBe ("player_id" -> "42")
  }
