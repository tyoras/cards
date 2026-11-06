package io.tyoras.cards.tests.endpoint.games.war

import io.circe.testing.golden.GoldenCodecTests
import io.tyoras.cards.tests.SharedCodecTestUtils
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FlatSpecDiscipline

import cats.data.NonEmptyList
import io.circe.parser.parse
import io.circe.syntax.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.endpoint.games.war.Payloads.Request
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID

class PayloadsSpec extends AnyFlatSpec with Matchers with EitherValues with SharedCodecTestUtils with FlatSpecDiscipline with Configuration:

  "War Creation request" should "round-trip through JSON" in {
    val creation = Request.Creation(NonEmptyList.of(User.ID(UUID.randomUUID()), User.ID(UUID.randomUUID())))

    creation.asJson.as[Request.Creation].value shouldBe creation
  }

  it should "decode player ids as flat UUIDs" in {
    val id      = UUID.randomUUID()
    val decoded = parse(s"""{"players":["$id"]}""").flatMap(_.as[Request.Creation])

    decoded.value shouldBe Request.Creation(NonEmptyList.one(User.ID(id)))
  }

  it should "reject an empty player list" in {
    parse("""{"players":[]}""").flatMap(_.as[Request.Creation]).isLeft shouldBe true
  }

  // golden JSON codec tests
  checkAll("GoldenCodec[Creation]", GoldenCodecTests[Request.Creation].goldenCodec)
