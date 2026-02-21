package io.tyoras.cards.domain.game.war

import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.game.war.model.*
import cats.syntax.all.*

import scala.util.control.NoStackTrace

val warDeck = international52Deck

extension (s: GameState)
  def pickFirstCard(playerId: PlayerId): Option[Card] =
    s.context.pickFirstCard(playerId)

opaque type PlayerCount = Int
object PlayerCount:
  enum ValidationError extends NoStackTrace:
    case InvalidNumber(msg: String)
    case OutOfBounds(count: Int)

  def from(count: Int): Either[ValidationError, PlayerCount] =
    Either.cond(2 <= count && count <= 52, count, ValidationError.OutOfBounds(count))

  def from(str: String): Either[ValidationError, PlayerCount] =
    Either.catchOnly[NumberFormatException](str.toInt).leftMap(_ => ValidationError.InvalidNumber(str)).flatMap(PlayerCount.from)

extension (pc: PlayerCount) def value: Int = pc
