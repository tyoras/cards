package io.tyoras.cards.server.endpoints.utils

import cats.effect.{IO, Ref}
import fs2.Stream
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.domain.user.model.User

import java.time.ZonedDateTime
import java.util.UUID

/** User service storing the users in memory */
final class InMemoryUserService(val store: Ref[IO, Map[User.ID, User.Existing]]) extends UserService[IO]:
  private val sorted: Map[User.ID, User.Existing] => List[User.Existing] = _.values.toList.sortBy(u => u.name: String)

  override def create(user: User.Data, withId: Option[User.ID]): IO[User.Existing] =
    val now     = ZonedDateTime.now()
    val created = User.Existing(withId.getOrElse(User.ID(UUID.randomUUID())), now, now, user)
    store.update(_.updated(created.id, created)).as(created)

  override def readById(id: User.ID): IO[Option[User.Existing]]          = store.get.map(_.get(id))
  override def readByName(name: User.Name): IO[Option[User.Existing]]    = store.get.map(_.values.find(_.name == name))
  override def readManyById(ids: List[User.ID]): IO[List[User.Existing]] = store.get.map(users => ids.flatMap(users.get))

  override def readManyByPartialName(name: User.Name): IO[List[User.Existing]] =
    store.get.map(sorted).map(_.filter(_.name.toLowerCase.contains(name.toLowerCase)))

  override def readAll: Stream[IO, User.Existing]               = Stream.evalSeq(store.get.map(sorted))
  override def update(user: User.Existing): IO[User.Existing]   = store.update(_.updated(user.id, user)).as(user)
  override def delete(user: User.Existing): IO[Unit]            = deleteMany(List(user))
  override def deleteMany(users: List[User.Existing]): IO[Unit] = store.update(_ -- users.map(_.id))
  override def deleteAll: IO[Unit]                              = store.set(Map.empty)

object InMemoryUserService:
  def make(users: User.Existing*): IO[InMemoryUserService] =
    Ref.of[IO, Map[User.ID, User.Existing]](users.map(u => u.id -> u).toMap).map(new InMemoryUserService(_))
