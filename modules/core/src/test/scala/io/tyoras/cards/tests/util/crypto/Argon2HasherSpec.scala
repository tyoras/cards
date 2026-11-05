package io.tyoras.cards.tests.util.crypto

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import io.tyoras.cards.util.crypto.Argon2Hasher
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class Argon2HasherSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val password = "correct horse battery staple"
  private val salt     = Array.tabulate[Byte](16)(_.toByte)

  "hash" should "produce a 32 bytes hash with a generated 16 bytes salt" in {
    Argon2Hasher.make[IO].flatMap(_.hash(password)).asserting { hashed =>
      hashed.hash should have size 32
      hashed.salt should have size 16
    }
  }

  it should "generate a different salt, hence a different hash, on each call" in {
    for
      hasher <- Argon2Hasher.make[IO]
      first  <- hasher.hash(password)
      second <- hasher.hash(password)
    yield
      first.salt.toSeq should not be second.salt.toSeq
      first.hash.toSeq should not be second.hash.toSeq
  }

  it should "be deterministic for a given salt, which it keeps" in {
    for
      hasher <- Argon2Hasher.make[IO]
      first  <- hasher.hash(password, Some(salt))
      second <- hasher.hash(password, Some(salt))
    yield
      first.salt.toSeq shouldBe salt.toSeq
      first.hash.toSeq shouldBe second.hash.toSeq
  }

  it should "produce a different hash for another password" in {
    for
      hasher <- Argon2Hasher.make[IO]
      first  <- hasher.hash(password, Some(salt))
      second <- hasher.hash("another password", Some(salt))
    yield first.hash.toSeq should not be second.hash.toSeq
  }

  "verify" should "accept the right password" in {
    for
      hasher <- Argon2Hasher.make[IO]
      hashed <- hasher.hash(password)
      result <- hasher.verify(password, hashed)
    yield result shouldBe true
  }

  it should "refuse a wrong password" in {
    for
      hasher <- Argon2Hasher.make[IO]
      hashed <- hasher.hash(password)
      result <- hasher.verify("wrong password", hashed)
    yield result shouldBe false
  }
