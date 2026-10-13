package io.tyoras.cards.tests.domain.auth

import cats.effect.IO
import cats.effect.Ref
import cats.effect.testing.scalatest.AsyncIOSpec
import fs2.Stream
import io.chrisdavenport.fuuid.FUUID
import io.github.iltotore.iron.*
import io.tyoras.cards.domain.auth.UserAuthRepository
import io.tyoras.cards.domain.auth.UserAuthService
import io.tyoras.cards.domain.auth.model.*
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.util.crypto.Argon2Hasher
import io.tyoras.cards.util.crypto.Argon2Hasher.Hashed
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.nio.charset.StandardCharsets.UTF_8
import java.time.ZonedDateTime
import java.util.UUID

class UserAuthServiceSpec extends AsyncWordSpec with AsyncIOSpec with Matchers with EitherValues:

  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val now             = ZonedDateTime.now()
  private val userId          = FUUID.fromUUID(UUID.randomUUID())
  private val userData        = User.Data(User.Name("Alice"), User.About("A test user"))
  private val user            = User.Existing(userId, now, now, userData)
  private val password        = Password("correct-password")
  private val wrongPassword   = Password("incorrect-password")
  private val newPassword     = Password("new-correct-password")
  private val passwordSalt    = Array[Byte](1, 2, 3, 4)
  private val updatedSalt     = Array[Byte](5, 6, 7, 8)
  private val initialAuthData = UserAuth.Data(
    userId,
    UserAuth.PasswordHashBytes.assume(password.getBytes(UTF_8)),
    UserAuth.PasswordSaltBytes.assume(passwordSalt)
  )
  private val initialAuth = UserAuth.Existing(now, now, initialAuthData)

  private case class Fixture(
      service: UserAuthService[IO],
      inserted: Ref[IO, List[UserAuth.Data]],
      updated: Ref[IO, List[UserAuth.Existing]],
      hashCalls: Ref[IO, List[(String, Option[Array[Byte]])]]
  )

  private def unsupported[A](method: String): IO[A] =
    IO.raiseError(new UnsupportedOperationException(method))

  private def fixture(
      createUser: User.Data => IO[User.Existing] = _ => IO.pure(user),
      readAuth: User.ID => IO[Option[UserAuth.Existing]] = _ => IO.pure(Some(initialAuth)),
      insertAuth: UserAuth.Data => IO[UserAuth.Existing] = data => IO.pure(UserAuth.Existing(now, now, data)),
      updateAuth: UserAuth.Existing => IO[UserAuth.Existing] = IO.pure,
      hashPassword: (String, Option[Array[Byte]]) => IO[Hashed] = (rawPassword, suppliedSalt) =>
        IO.pure(Hashed(rawPassword.getBytes(UTF_8), suppliedSalt.getOrElse(passwordSalt)))
  ): IO[Fixture] =
    for
      inserted  <- Ref.of[IO, List[UserAuth.Data]](Nil)
      updated   <- Ref.of[IO, List[UserAuth.Existing]](Nil)
      hashCalls <- Ref.of[IO, List[(String, Option[Array[Byte]])]](Nil)
    yield
      val users = new UserService[IO]:
        override def create(data: User.Data, withId: Option[FUUID]): IO[User.Existing] = createUser(data)
        override def readById(id: FUUID): IO[Option[User.Existing]]                    = unsupported("readById")
        override def readByName(name: User.Name): IO[Option[User.Existing]]            = unsupported("readByName")
        override def readManyById(ids: List[FUUID]): IO[List[User.Existing]]           = unsupported("readManyById")
        override def readManyByPartialName(name: User.Name): IO[List[User.Existing]]   = unsupported("readManyByPartialName")
        override def readAll: Stream[IO, User.Existing]                                = Stream.empty
        override def update(existing: User.Existing): IO[User.Existing]                = unsupported("update")
        override def delete(existing: User.Existing): IO[Unit]                         = unsupported("delete")
        override def deleteMany(existing: List[User.Existing]): IO[Unit]               = unsupported("deleteMany")
        override def deleteAll: IO[Unit]                                               = unsupported("deleteAll")

      val repository = new UserAuthRepository[IO]:
        override def insert(auth: UserAuth.Data): IO[UserAuth.Existing] =
          inserted.update(_ :+ auth) *> insertAuth(auth)
        override def readByUserId(id: User.ID): IO[Option[UserAuth.Existing]] = readAuth(id)
        override def update(auth: UserAuth.Existing): IO[UserAuth.Existing]   =
          updated.update(_ :+ auth) *> updateAuth(auth)
        override def delete(auth: UserAuth.Existing): IO[Unit] = unsupported("delete")
        override def deleteAll: IO[Unit]                       = unsupported("deleteAll")

      val hasher = new Argon2Hasher[IO]:
        override def hash(rawPassword: String, salt: Option[Array[Byte]]): IO[Hashed] =
          hashCalls.update(_ :+ (rawPassword -> salt)) *> hashPassword(rawPassword, salt)
        override def verify(rawPassword: String, hashed: Hashed): IO[Boolean] = unsupported("verify")

      Fixture(UserAuthService.of(users, repository, hasher), inserted, updated, hashCalls)

  "UserAuthService.createNewUser" should {
    "create the user, hash the password, and persist credentials for that user" in {
      for
        f      <- fixture()
        result <- f.service.createNewUser(userData, password)
        saved  <- f.inserted.get
      yield
        result shouldBe user
        saved should have size 1
        saved.head.userId shouldBe user.id
        saved.head.passwordHash.toSeq shouldBe password.getBytes(UTF_8).toSeq
        saved.head.passwordSalt.toSeq shouldBe passwordSalt.toSeq
    }

    "translate user creation errors and skip hashing and persistence" in {
      val f = fixture(createUser = _ => IO.raiseError(new RuntimeException("user create failed")))
      for
        harness <- f
        result  <- harness.service.createNewUser(userData, password).attempt
        hashes  <- harness.hashCalls.get
        saved   <- harness.inserted.get
      yield
        result.left.value shouldBe UserAuthError.UserCreationFailure
        hashes shouldBe empty
        saved shouldBe empty
    }

    "reject an empty password hash" in {
      val f = fixture(hashPassword = (_, salt) => IO.pure(Hashed(Array.emptyByteArray, salt.getOrElse(passwordSalt))))
      for
        harness <- f
        result  <- harness.service.createNewUser(userData, password).attempt
        saved   <- harness.inserted.get
      yield
        result.left.value should matchPattern { case UserAuthError.InvalidHash(_) => }
        saved shouldBe empty
    }

    "reject an empty password salt" in {
      val f = fixture(hashPassword = (rawPassword, _) => IO.pure(Hashed(rawPassword.getBytes(UTF_8), Array.emptyByteArray)))
      for
        harness <- f
        result  <- harness.service.createNewUser(userData, password).attempt
        saved   <- harness.inserted.get
      yield
        result.left.value should matchPattern { case UserAuthError.InvalidSalt(_) => }
        saved shouldBe empty
    }

    "translate credential persistence errors" in {
      val f = fixture(insertAuth = _ => IO.raiseError(new RuntimeException("insert failed")))
      for
        harness <- f
        result  <- harness.service.createNewUser(userData, password).attempt
      yield result.left.value shouldBe UserAuthError.CredentialPersistenceFailure
    }
  }

  "UserAuthService.changeUserPassword" should {
    "fail when the user has no stored credentials" in {
      val f = fixture(readAuth = _ => IO.pure(None))
      for
        harness <- f
        result  <- harness.service.changeUserPassword(userId, password, newPassword).attempt
        hashes  <- harness.hashCalls.get
        updates <- harness.updated.get
      yield
        result.left.value shouldBe UserAuthError.UserNotFound(userId)
        hashes shouldBe empty
        updates shouldBe empty
    }

    "reject an incorrect old password without hashing the new password or updating credentials" in {
      val f = fixture()
      for
        harness <- f
        result  <- harness.service.changeUserPassword(userId, wrongPassword, newPassword).attempt
        hashes  <- harness.hashCalls.get
        updates <- harness.updated.get
      yield
        result.left.value shouldBe UserAuthError.InvalidOldPassword
        hashes.map(_._1) shouldBe List(wrongPassword)
        hashes.head._2.map(_.toSeq) shouldBe Some(passwordSalt.toSeq)
        updates shouldBe empty
    }

    "rehash and update credentials when the old password is correct" in {
      val f = fixture(hashPassword = (rawPassword, salt) => IO.pure(Hashed(rawPassword.getBytes(UTF_8), salt.getOrElse(updatedSalt))))
      for
        harness <- f
        result  <- harness.service.changeUserPassword(userId, password, newPassword).attempt
        calls   <- harness.hashCalls.get
        updates <- harness.updated.get
      yield
        result shouldBe Right(())
        calls.map(_._1) shouldBe List(password, newPassword)
        calls.head._2.map(_.toSeq) shouldBe Some(passwordSalt.toSeq)
        calls(1)._2 shouldBe None
        updates should have size 1
        updates.head.userId shouldBe userId
        updates.head.passwordHash.toSeq shouldBe newPassword.getBytes(UTF_8).toSeq
        updates.head.passwordSalt.toSeq shouldBe updatedSalt.toSeq
        updates.head.createdAt shouldBe initialAuth.createdAt
        updates.head.updatedAt shouldBe initialAuth.updatedAt
    }

    "reject an empty generated password hash" in {
      val f = fixture(hashPassword =
        (rawPassword, salt) =>
          if salt.isDefined then IO.pure(Hashed(rawPassword.getBytes(UTF_8), salt.get))
          else IO.pure(Hashed(Array.emptyByteArray, updatedSalt))
      )
      for
        harness <- f
        result  <- harness.service.changeUserPassword(userId, password, newPassword).attempt
        updates <- harness.updated.get
      yield
        result.left.value should matchPattern { case UserAuthError.InvalidHash(_) => }
        updates shouldBe empty
    }

    "reject an empty generated password salt" in {
      val f = fixture(hashPassword =
        (rawPassword, salt) =>
          if salt.isDefined then IO.pure(Hashed(rawPassword.getBytes(UTF_8), salt.get))
          else IO.pure(Hashed(rawPassword.getBytes(UTF_8), Array.emptyByteArray))
      )
      for
        harness <- f
        result  <- harness.service.changeUserPassword(userId, password, newPassword).attempt
        updates <- harness.updated.get
      yield
        result.left.value should matchPattern { case UserAuthError.InvalidSalt(_) => }
        updates shouldBe empty
    }
  }
