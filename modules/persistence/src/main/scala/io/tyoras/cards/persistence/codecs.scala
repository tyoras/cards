package io.tyoras.cards.persistence.codecs

import io.tyoras.cards.domain.game.GameType
import io.tyoras.cards.domain.game.GameTyp.*
import _root_.skunk.Codec
import _root_.skunk.codec.all.*
import _root_.skunk.data.Type

import java.time.{ZoneOffset, ZonedDateTime}

object skunk:
  val timestampTZ: Codec[ZonedDateTime] = timestamptz.imap(
    _.atZoneSameInstant(ZoneOffset.UTC)
  )(
    _.withZoneSameInstant(ZoneOffset.UTC).toOffsetDateTime
  )

  val gameType: Codec[GameType] = `enum`[GameType](
    _.label,
    {
      case Schnapsen.label => Some(Schnapsen)
      case War.label       => Some(War)
      case _               => None
    },
    Type("game_type")
  )
