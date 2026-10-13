package io.tyoras.cards.tests.domain.auth

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import dev.profunktor.auth.jwt.JwtToken
import fs2.Stream
import io.chrisdavenport.fuuid.FUUID
import io.github.iltotore.iron.autoRefine
import io.circe.syntax.*
import io.tyoras.cards.domain.auth.{AuthService, JWTGenerator, JwtExpiration, UserAuthRepository}
import io.tyoras.cards.domain.auth.config.AuthConfig
import io.tyoras.cards.domain.auth.model.*
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.util.crypto.Argon2Hasher
import io.tyoras.cards.util.crypto.Argon2Hasher.Hashed
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import pdi.jwt.JwtClaim

import java.nio.charset.StandardCharsets.UTF_8
import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.duration.*

class AuthServiceSpec extends AsyncWordSpec with AsyncIOSpec with Matchers with EitherValues:

  private val now  = ZonedDateTime.now()
  private val user = User.Existing(
    FUUID.fromUUID(UUID.randomUUID()),
    now,
    now,
    User.Data(User.Name("Alice"), User.About("A test user"))
  )
  private val password     = Password("correct-password")
  private val passwordSalt = Array[Byte](1, 2, 3, 4)
  private val config       = AuthConfig("test-secret-key-that-is-at-least-32-bytes-long", TokenExpiration.assume(5.minutes))
  private val dummyToken   = JwtToken("dummy-token")

  private val hasher: Argon2Hasher[IO] = new Argon2Hasher[IO]:
    override def hash(password: String, salt: Option[Array[Byte]]): IO[Hashed] =
      IO.pure(Hashed(password.getBytes(UTF_8), salt.getOrElse(passwordSalt)))

    override def verify(password: String, hashed: Hashed): IO[Boolean] =
      hash(password, Some(hashed.salt)).map(_.hash.sameElements(hashed.hash))

  private val jwtExpiration: JwtExpiration[IO]   = (claim, _) => IO.pure(claim)
  private val realJwtGenerator: JWTGenerator[IO] = JWTGenerator.make(jwtExpiration, config)

  private def credentials(forUser: User.Existing, passwordHash: String = password): UserAuth.Existing =
    UserAuth.Existing(
      now,
      now,
      UserAuth.Data(
        forUser.id,
        UserAuth.PasswordHashBytes.assume(passwordHash.getBytes(UTF_8)),
        UserAuth.PasswordSaltBytes.assume(passwordSalt)
      )
    )

  private def makeService(
      users: List[User.Existing] = List(user),
      userCredentials: List[UserAuth.Existing] = List(credentials(user)),
      jwtGenerator: JWTGenerator[IO] = (_: User.Existing) => IO.pure(dummyToken)
  ): IO[AuthService[IO]] =
    val userService: UserService[IO] = new UserService[IO]:
      override def create(data: User.Data, withId: Option[FUUID]): IO[User.Existing] =
        IO.raiseError(new UnsupportedOperationException("create"))

      override def readById(id: FUUID): IO[Option[User.Existing]] =
        IO.pure(users.find(_.id == id))

      override def readByName(name: User.Name): IO[Option[User.Existing]] =
        IO.pure(users.find(_.name == name))

      override def readManyById(ids: List[FUUID]): IO[List[User.Existing]] =
        IO.pure(users.filter(u => ids.contains(u.id)))

      override def readManyByPartialName(name: User.Name): IO[List[User.Existing]] =
        IO.pure(users.filter(_.name.contains(name)))

      override def readAll: Stream[IO, User.Existing] =
        Stream.emits(users)

      override def update(existing: User.Existing): IO[User.Existing] =
        IO.pure(existing)

      override def delete(existing: User.Existing): IO[Unit] =
        IO.unit

      override def deleteMany(existing: List[User.Existing]): IO[Unit] =
        IO.unit

      override def deleteAll: IO[Unit] =
        IO.unit

    val repository: UserAuthRepository[IO] = new UserAuthRepository[IO]:
      override def insert(auth: UserAuth.Data): IO[UserAuth.Existing] =
        IO.raiseError(new UnsupportedOperationException("insert"))

      override def readByUserId(userId: User.ID): IO[Option[UserAuth.Existing]] =
        IO.pure(userCredentials.find(_.userId == userId))

      override def update(auth: UserAuth.Existing): IO[UserAuth.Existing] =
        IO.raiseError(new UnsupportedOperationException("update"))

      override def delete(auth: UserAuth.Existing): IO[Unit] =
        IO.unit

      override def deleteAll: IO[Unit] =
        IO.unit

    AuthService.make(userService, jwtGenerator, hasher, repository, config)

  "AuthService" should {
    "return the user and a token when the supplied password is correct" in {
      for
        service <- makeService()
        result  <- service.login(LoginAttempt(user.name, password))
      yield result shouldBe LoginSuccess(dummyToken, user)
    }

    "fail login for an unknown username" in {
      val unknownName = User.Name("Nobody")
      for
        service <- makeService()
        result  <- service.login(LoginAttempt(unknownName, password)).attempt
      yield result.left.value shouldBe AuthError.UnknownUser(unknownName)
    }

    "fail login when the user has no stored credentials" in {
      for
        service <- makeService(userCredentials = Nil)
        result  <- service.login(LoginAttempt(user.name, password)).attempt
      yield result.left.value shouldBe AuthError.LoginFailure(user.id, "missing credentials")
    }

    "fail login when the password does not match the stored hash" in {
      val wrongPassword = Password("incorrect-password")
      for
        service <- makeService()
        result  <- service.login(LoginAttempt(user.name, wrongPassword)).attempt
      yield result.left.value shouldBe AuthError.LoginFailure(user.id, "wrong password")
    }

    "resolve a valid user claim with the authenticator" in {
      for
        service <- makeService()
        result  <- service.authenticator(dummyToken)(JwtClaim(UserClaim(user.id).asJson.noSpaces))
      yield result shouldBe Some(user)
    }

    "return no user for a malformed claim" in {
      for
        service <- makeService()
        result  <- service.authenticator(dummyToken)(JwtClaim("not-json"))
      yield result shouldBe None
    }

    "authenticate a signed token and return its user" in {
      for
        service <- makeService(jwtGenerator = realJwtGenerator)
        token   <- realJwtGenerator.create(user)
        result  <- service.authenticate(token)
      yield result shouldBe user
    }

    "reject a token that cannot be decoded" in {
      for
        service <- makeService()
        result  <- service.authenticate(JwtToken("not-a-valid-jwt")).attempt
      yield result.left.value should matchPattern { case AuthError.InvalidToken(_) => }
    }

    "fail authentication when a valid token refers to an unknown user" in {
      val missingUser = user.copy(id = FUUID.fromUUID(UUID.randomUUID()))
      for
        service <- makeService(users = Nil, userCredentials = Nil, jwtGenerator = realJwtGenerator)
        token   <- realJwtGenerator.create(missingUser)
        result  <- service.authenticate(token).attempt
      yield result.left.value shouldBe AuthError.UnknownUser(User.Name("user from jwt"))
    }
  }
