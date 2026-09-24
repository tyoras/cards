package io.tyoras.cards.tests.domain.card

import io.circe.testing.golden.GoldenCodecTests
import io.tyoras.cards.domain.card.{Card, Rank, Suit}
import io.tyoras.cards.tests.CodecTestUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FlatSpecDiscipline
import io.tyoras.cards.domain.card.codecs.given

class JsonCodecsSpec extends AnyFlatSpec with CodecTestUtils with FlatSpecDiscipline with Configuration:

  checkAll("GoldenCodec[Card]", GoldenCodecTests[Card].goldenCodec)
  checkAll("GoldenCodec[Suit]", GoldenCodecTests[Suit].goldenCodec)
  checkAll("GoldenCodec[Rank]", GoldenCodecTests[Rank].goldenCodec)
