package io.tyoras.cards.persistence

import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.game.GameType
import io.tyoras.cards.domain.game.GameType.*
import io.tyoras.cards.util.error.TechnicalError
import skunk.Codec
import skunk.codec.all.*
import skunk.data.Type

import java.time.{ZoneOffset, ZonedDateTime}

val fuuid: Codec[FUUID] = Codec.simple[FUUID](
  u => u.show,
  s => FUUID.fromString(s).leftMap(_.getMessage),
  Type.uuid
)
val timestampTZ: Codec[ZonedDateTime] = timestamptz.imap(
  _.atZoneSameInstant(ZoneOffset.UTC)
)(
  _.withZoneSameInstant(ZoneOffset.UTC).toOffsetDateTime
)

val gameType: Codec[GameType[?, ?]] = `enum`[GameType[?, ?]](
  _.label,
  {
    case Schnapsen.label => Some(Schnapsen)
    case War.label       => Some(War)
    case _               => None
  },
  Type("game_type")
)

case class PersistenceError(override val code: String, msg: String) extends TechnicalError(code, msg)
