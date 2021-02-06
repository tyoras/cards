package io.tyoras.cards

import skunk.Codec
import skunk.codec.all.timestamptz

import java.time.{ZoneOffset, ZonedDateTime}

package object persistence {
  val timestampTZ: Codec[ZonedDateTime] = timestamptz.imap(_.atZoneSameInstant(ZoneOffset.UTC))(_.withZoneSameInstant(ZoneOffset.UTC).toOffsetDateTime)

}
