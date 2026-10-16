package io.tyoras.cards.tests.domain.auth

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import dev.profunktor.auth.jwt.{JwtAuth, jwtDecode}
import io.circe.parser.decode
import io.github.iltotore.iron.autoRefine
import io.tyoras.cards.domain.auth.{JWTGenerator, JwtExpiration}
import io.tyoras.cards.domain.auth.config.AuthConfig
import io.tyoras.cards.domain.auth.model.{TokenExpiration, UserClaim}
import io.tyoras.cards.domain.user.model.User
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import java.time.{Clock, ZonedDateTime}
import java.util.UUID
import scala.concurrent.duration.*
import pl.iterators.kebs.circe.snakified.given

class JWTGeneratorSpec extends AsyncWordSpec with AsyncIOSpec with Matchers:

  private val now: ZonedDateTime = ZonedDateTime.now()

  private val user: User.Existing = User.Existing(
    User.ID(UUID.randomUUID()),
    now,
    now,
    User.Data(User.Name("Alice"), User.About("A test user"))
  )

  private val config: AuthConfig = AuthConfig("super-secret-key", TokenExpiration.assume(5.minutes))

  private val jwtExpiration: JwtExpiration[IO] = new JwtExpiration[IO]:
    override def expiresIn(claim: pdi.jwt.JwtClaim, exp: TokenExpiration): IO[pdi.jwt.JwtClaim] =
      IO.delay {
        given Clock = Clock.systemUTC()
        claim.issuedNow.expiresIn(exp.toMillis)
      }

  "JWTGenerator" should {
    "create a token that can be decoded with the configured secret" in {
      val jwtAuth = JwtAuth.hmac(config.secretKey.toCharArray, config.hmacAlgo)

      for
        token     <- JWTGenerator.make[IO](jwtExpiration, config).create(user)
        claim     <- jwtDecode[IO](token, jwtAuth)
        userClaim <- IO.fromEither(decode[UserClaim](claim.content))
      yield userClaim.userId shouldBe user.id
    }

    "encode the user id in the JWT payload" in {
      val jwtAuth   = JwtAuth.hmac(config.secretKey.toCharArray, config.hmacAlgo)
      val otherUser = user.copy(id = User.ID(UUID.randomUUID()))

      for
        token          <- JWTGenerator.make[IO](jwtExpiration, config).create(user)
        otherToken     <- JWTGenerator.make[IO](jwtExpiration, config).create(otherUser)
        claim          <- jwtDecode[IO](token, jwtAuth)
        otherClaim     <- jwtDecode[IO](otherToken, jwtAuth)
        userClaim      <- IO.fromEither(decode[UserClaim](claim.content))
        otherUserClaim <- IO.fromEither(decode[UserClaim](otherClaim.content))
      yield {
        userClaim.userId shouldBe user.id
        otherUserClaim.userId shouldBe otherUser.id
        otherUserClaim.userId should not be userClaim.userId
      }
    }
  }
