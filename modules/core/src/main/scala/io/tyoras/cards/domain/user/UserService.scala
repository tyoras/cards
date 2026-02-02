package io.tyoras.cards.domain.user

import cats.Monad
import cats.effect.Clock
import cats.syntax.all.*
import io.chrisdavenport.cats.effect.time.implicits.ClockOps
import io.chrisdavenport.fuuid.FUUID

trait UserService[F[_]]:
  def create(user: User.Data, withId: Option[FUUID] = None): F[User.Existing]

  def createMany(users: List[User.Data]): F[List[User.Existing]]

  def readById(id: FUUID): F[Option[User.Existing]]

  def readManyById(ids: List[FUUID]): F[List[User.Existing]]

  def readManyByName(name: String): F[List[User.Existing]]

  def readAll: F[List[User.Existing]]

  def update(user: User.Existing): F[User.Existing]

  def updateMany(users: List[User.Existing]): F[List[User.Existing]]

  def delete(user: User.Existing): F[Unit]

  def deleteMany(users: List[User.Existing]): F[Unit]

  def deleteAll: F[Unit]

object UserService:
  def of[F[_] : Monad : Clock](userRepo: UserRepository[F]): UserService[F] = new:
    override def create(user: User.Data, withId: Option[FUUID]): F[User.Existing] =
      userRepo.insert(user, withId)

    override def createMany(Users: List[User.Data]): F[List[User.Existing]] =
      writeMany(Users)

    private def writeMany[T <: User](Users: List[T]): F[List[User.Existing]] =
      Clock[F].getZonedDateTimeUTC.flatMap { now =>
        userRepo.writeMany(
          Users.map(User => User.withUpdatedName(User.name.trim, now))
        )
      }

    override def readById(id: FUUID): F[Option[User.Existing]] =
      readManyById(List(id)).map(_.headOption)

    override def readManyById(ids: List[FUUID]): F[List[User.Existing]] =
      userRepo.readManyById(ids)

    override def readManyByName(name: String): F[List[User.Existing]] =
      if name.isEmpty then List.empty.pure[F]
      else userRepo.readManyByPartialName(name.trim)

    override val readAll: F[List[User.Existing]] =
      userRepo.readAll

    override def update(user: User.Existing): F[User.Existing] =
      updateMany(List(user)).map(_.head)

    override def updateMany(users: List[User.Existing]): F[List[User.Existing]] =
      writeMany(users)

    override def delete(user: User.Existing): F[Unit] =
      deleteMany(List(user))

    override def deleteMany(users: List[User.Existing]): F[Unit] =
      userRepo.deleteMany(users)

    override val deleteAll: F[Unit] =
      userRepo.deleteAll
