package io.tyoras.cards.tests.domain.auth

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import io.tyoras.cards.domain.auth.JwtExpiration
import io.tyoras.cards.domain.auth.model.TokenExpiration
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import pdi.jwt.JwtClaim

import scala.concurrent.duration.*

class JwtExpirationSpec extends AsyncWordSpec with AsyncIOSpec with Matchers with OptionValues:

  "JwtExpiration" should {
    "set the expiration claim to now plus the configured duration" in {
      val claim      = JwtClaim("user-id")
      val expiration = TokenExpiration.assume(5.minutes)

      JwtExpiration.make[IO].flatMap { jwtExpiration =>
        jwtExpiration.expiresIn(claim, expiration).map { updated =>
          updated.expiration should not be empty

          val nowSeconds  = System.currentTimeMillis() / 1_000L
          val expectedMin = nowSeconds + expiration.toSeconds - 5L
          val expectedMax = nowSeconds + expiration.toSeconds + 5L
          val actual      = updated.expiration.value

          actual should be >= expectedMin
          actual should be <= expectedMax
        }
      }
    }
  }
