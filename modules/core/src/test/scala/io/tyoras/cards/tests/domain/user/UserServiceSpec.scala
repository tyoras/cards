package io.tyoras.cards.tests.domain.user

import cats.effect.{IO, Ref}
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.show.*
import fs2.Stream
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.domain.user.{UserRepository, UserService}
import io.tyoras.cards.tests.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{ZoneOffset, ZonedDateTime}

class UserServiceSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val now   = ZonedDateTime.of(2026, 1, 1, 12, 0, 0, 0, ZoneOffset.UTC)
  private val alice = User.Data(User.Name("Alice"), User.About("First user"))
  private val bob   = User.Data(User.Name("Bob"), User.About("Second user"))

  private def existing(data: User.Data): User.Existing = User.Existing(userIdGen.pick, now, now, data)

  private class InMemoryRepo(users: Ref[IO, List[User.Existing]]) extends UserRepository[IO]:
    override def writeMany(toWrite: List[User]): IO[List[User.Existing]] =
      val written = toWrite.map {
        case e: User.Existing => e
        case d: User.Data     => existing(d)
      }
      users.update(current => current.filterNot(c => written.exists(_.id == c.id)) ++ written).as(written)

    override def insert(data: User.Data, withId: Option[User.ID]): IO[User.Existing] =
      val user = User.Existing(withId.getOrElse(userIdGen.pick), now, now, data)
      users.update(_ :+ user).as(user)

    override def readManyById(ids: List[User.ID]): IO[List[User.Existing]] = users.get.map(_.filter(u => ids.contains(u.id)))

    override def readManyByPartialName(name: User.Name): IO[List[User.Existing]] = users.get.map(_.filter(_.name.contains(name)))

    override def readManyByName(names: List[User.Name]): IO[List[User.Existing]] = users.get.map(_.filter(u => names.contains(u.name)))

    override def readAll: Stream[IO, User.Existing] = Stream.evalSeq(users.get)

    override def deleteMany(toDelete: List[User.Existing]): IO[Unit] = users.update(_.filterNot(u => toDelete.exists(_.id == u.id)))

    override def deleteAll: IO[Unit] = users.set(Nil)

  private case class Fixture(service: UserService[IO], repo: UserRepository[IO], users: Ref[IO, List[User.Existing]])

  private def fixture(initial: User.Existing*): IO[Fixture] =
    Ref.of[IO, List[User.Existing]](initial.toList).map { users =>
      val repo = new InMemoryRepo(users)
      Fixture(UserService.of[IO](repo), repo, users)
    }

  "create" should "persist a new user with a generated id by default" in {
    for
      f       <- fixture()
      created <- f.service.create(alice)
      stored  <- f.users.get
    yield
      created.data shouldBe alice
      stored shouldBe List(created)
  }

  it should "use the provided id" in {
    val id = userIdGen.pick
    fixture().flatMap(_.service.create(alice, Some(id))).asserting(_.id shouldBe id)
  }

  "UserRepository.insert" should "not force an id by default" in
    fixture().flatMap(_.repo.insert(alice)).asserting(_.data shouldBe alice)

  "readById" should "return the user when it exists" in {
    val user = existing(alice)
    fixture(user, existing(bob)).flatMap(_.service.readById(user.id)).asserting(_ shouldBe Some(user))
  }

  it should "return nothing when the user does not exist" in
    fixture().flatMap(_.service.readById(userIdGen.pick)).asserting(_ shouldBe None)

  "readByName" should "return the user with this exact name" in {
    val user = existing(alice)
    fixture(user, existing(bob)).flatMap(_.service.readByName(alice.name)).asserting(_ shouldBe Some(user))
  }

  it should "return nothing when no user has this name" in
    fixture(existing(bob)).flatMap(_.service.readByName(alice.name)).asserting(_ shouldBe None)

  "readManyById" should "return the users with these ids" in {
    val user1 = existing(alice)
    val user2 = existing(bob)
    fixture(user1, user2).flatMap(_.service.readManyById(List(user2.id))).asserting(_ shouldBe List(user2))
  }

  "readManyByPartialName" should "return the users with a matching name" in {
    val user = existing(alice)
    fixture(user, existing(bob)).flatMap(_.service.readManyByPartialName(User.Name("lic"))).asserting(_ shouldBe List(user))
  }

  "readAll" should "stream all the users" in {
    val users = List(existing(alice), existing(bob))
    fixture(users*).flatMap(_.service.readAll.compile.toList).asserting(_ shouldBe users)
  }

  "update" should "persist and return the user" in {
    val user    = existing(alice)
    val updated = user.withUpdatedAbout(User.About("Updated"))
    for
      f      <- fixture(user)
      result <- f.service.update(updated)
      stored <- f.users.get
    yield
      result shouldBe updated
      stored shouldBe List(updated)
  }

  "delete" should "remove only the given user" in {
    val user  = existing(alice)
    val other = existing(bob)
    for
      f      <- fixture(user, other)
      _      <- f.service.delete(user)
      stored <- f.users.get
    yield stored shouldBe List(other)
  }

  "deleteMany" should "remove the given users" in {
    val user1 = existing(alice)
    val user2 = existing(bob)
    for
      f      <- fixture(user1, user2)
      _      <- f.service.deleteMany(List(user1, user2))
      stored <- f.users.get
    yield stored shouldBe empty
  }

  "deleteAll" should "remove all the users" in {
    for
      f      <- fixture(existing(alice), existing(bob))
      _      <- f.service.deleteAll
      stored <- f.users.get
    yield stored shouldBe empty
  }

  "User.Data" should "update its name and description" in {
    alice.withUpdatedName(bob.name).name shouldBe bob.name
    alice.withUpdatedAbout(bob.about).about shouldBe bob.about
  }

  it should "be displayed" in {
    alice.show shouldBe "name = Alice | about = First user"
  }

  "User.Existing" should "update its name and description, keeping its identity" in {
    val user = existing(alice)
    user.withUpdatedName(bob.name) shouldBe user.copy(data = alice.copy(name = bob.name))
    user.withUpdatedAbout(bob.about) shouldBe user.copy(data = alice.copy(about = bob.about))
    user.name shouldBe alice.name
    user.about shouldBe alice.about
  }

  it should "be displayed" in {
    val user = existing(alice)
    user.show should include(s"id = ${user.id}")
    user.show should include("name = Alice")
  }

  "User.ID" should "be displayed like its value" in {
    val id = userIdGen.pick
    id.show shouldBe id.toString
  }
