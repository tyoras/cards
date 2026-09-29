package io.tyoras.cards.persistence.test.matchers

import org.scalatest.matchers.{MatchResult, Matcher}

import java.time.ZonedDateTime
import scala.concurrent.duration.*

// Dates
object DateMatcher:
  def beSameMillis(other: ZonedDateTime, deltaMillis: FiniteDuration = 5.millis): Matcher[ZonedDateTime] = (left: ZonedDateTime) =>
    MatchResult(
      Duration(left.getNano - other.getNano, NANOSECONDS) < deltaMillis,
      s"$left is not the same as $other",
      s"$left is the same as $other"
    )

  def beAfter(other: ZonedDateTime): Matcher[ZonedDateTime] = (left: ZonedDateTime) =>
    MatchResult(
      left.isAfter(other),
      s"$left is not after $other",
      s"$left is after $other"
    )

  def beBefore(other: ZonedDateTime): Matcher[ZonedDateTime] = (left: ZonedDateTime) =>
    MatchResult(
      left.isBefore(other),
      s"$left is not before $other",
      s"$left is before $other"
    )
