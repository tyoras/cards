package io.tyoras.cards

import cats.implicits.toBifunctorOps
import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.game.GameType
import skunk.Codec
import skunk.codec.all.*
import skunk.data.Type

import java.time.{ZoneOffset, ZonedDateTime}

package object persistence:
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

  val gameType: Codec[GameType] = `enum`[GameType](_.toString.toLowerCase, s => GameType.valueOf(s.capitalize).some, Type("game_type"))
