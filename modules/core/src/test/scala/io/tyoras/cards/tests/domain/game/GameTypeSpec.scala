package io.tyoras.cards.tests.domain.game

import io.tyoras.cards.domain.game.{GameTyp, GameType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.circe.testing.golden.GoldenCodecTests
import io.tyoras.cards.tests.CodecTestUtils
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FlatSpecDiscipline

class GameTypeSpec extends AnyFlatSpec with Matchers with CodecTestUtils with FlatSpecDiscipline with Configuration {
  checkAll("GoldenCodec[GameType]", GoldenCodecTests[GameType].goldenCodec)

  "Schnapsen" should "have correct label" in {
    GameTyp.Schnapsen.label should be("schnapsen")
  }

  it should "have correct player count range" in {
    GameTyp.Schnapsen.minPlayers should be(2)
    GameTyp.Schnapsen.maxPlayers should be(2)
  }

  "War" should "have correct label" in {
    GameTyp.War.label should be("war")
  }

  it should "have correct player count range" in {
    GameTyp.War.minPlayers should be(2)
    GameTyp.War.maxPlayers should be(52)
  }
}
