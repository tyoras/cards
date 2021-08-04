package io.tyoras.cards

import cats.implicits.toBifunctorOps
import io.chrisdavenport.fuuid.FUUID
import skunk.Codec
import skunk.codec.all.timestamptz
import skunk.data.Type

import java.time.{ZoneOffset, ZonedDateTime}

package object persistence {
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
}
