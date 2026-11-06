package io.tyoras.cards.cli.remote.config

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import org.http4s.implicits.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import pureconfig.ConfigSource

/** The env variables used by these tests are defined in the build (cli / Test / envVars) */
class ConfigSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  "parseConfig" should "load the cli configuration from the resources and the env variables" in {
    parseConfig[IO].asserting { config =>
      config.client shouldBe CardsClientConfig(uri"http://localhost:1", uri"ws://localhost:1")
      config.auth.userName shouldBe "TestUser"
      config.auth.password shouldBe "test-password"
    }
  }

  "RemoteCliConfig" should "reject an invalid password" in {
    val source = ConfigSource.string("""
      client { api-uri = "http://a", ws-uri = "ws://a" }
      auth { user-name = "Bob", password = "short" }
    """)
    IO(source.load[RemoteCliConfig]).asserting(_.isLeft shouldBe true)
  }

  it should "reject an invalid user name" in {
    val source = ConfigSource.string("""
      client { api-uri = "http://a", ws-uri = "ws://a" }
      auth { user-name = " ", password = "long-enough-password" }
    """)
    IO(source.load[RemoteCliConfig]).asserting(_.isLeft shouldBe true)
  }
