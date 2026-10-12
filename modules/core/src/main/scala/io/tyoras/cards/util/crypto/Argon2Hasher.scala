package io.tyoras.cards.util.crypto

import cats.effect.Sync
import cats.effect.std.SecureRandom
import io.tyoras.cards.util.crypto.Argon2Hasher.Hashed
import cats.syntax.all.*
import org.bouncycastle.crypto.generators.Argon2BytesGenerator
import org.bouncycastle.crypto.params.Argon2Parameters

import java.nio.charset.StandardCharsets.UTF_8

trait Argon2Hasher[F[_]]:
  def hash(password: String, salt: Option[Array[Byte]] = None): F[Hashed]

  def verify(password: String, hashed: Hashed): F[Boolean]

object Argon2Hasher:
  case class Hashed(hash: Array[Byte], salt: Array[Byte])
  private val saltLength: Int = 16  // bytes
  private val hashLength: Int = 32  // bytes
  private val iterations: Int = 2
  private val memory: Int     = 256 // KiB /!\ recommended value for prod usage is 65536 /!\
  private val parallelism     = 1

  def make[F[_] : Sync]: F[Argon2Hasher[F]] =
    for secureRandom <- SecureRandom.of[F]
    yield new Argon2Hasher[F]:
      override def hash(password: String, providedSalt: Option[Array[Byte]]): F[Hashed] =
        for
          salt      <- providedSalt.fold(generateSalt(saltLength))(_.pure)
          generator <- Sync[F].blocking(argon2Generator(salt))
          hash      <- Sync[F].blocking {
            val outputBytes = new Array[Byte](hashLength)
            generator.generateBytes(password.getBytes(UTF_8), outputBytes)
            outputBytes
          }
        yield Hashed(hash, salt)

      override def verify(password: String, hashed: Hashed): F[Boolean] =
        for hashedPassword <- hash(password, hashed.salt.some)
        yield hashedPassword.hash.sameElements(hashed.hash)

      private def generateSalt(length: Int): F[Array[Byte]] =
        secureRandom.nextBytes(length)

      private def argon2Generator(salt: Array[Byte]): Argon2BytesGenerator =
        val builder = new Argon2Parameters.Builder(Argon2Parameters.ARGON2_id)
          .withVersion(Argon2Parameters.ARGON2_VERSION_13)
          .withIterations(iterations)
          .withMemoryAsKB(memory)
          .withParallelism(parallelism)
          .withSalt(salt)

        val generator = new Argon2BytesGenerator()
        generator.init(builder.build())
        generator
