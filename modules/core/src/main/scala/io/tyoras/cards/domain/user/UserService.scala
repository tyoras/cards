package io.tyoras.cards.domain.user

import cats.Monad
import cats.effect.Clock
import io.chrisdavenport.cats.effect.time.implicits.ClockOps
import cats.syntax.all._

import java.util.UUID

trait UserService[F[_]] {
  def createOne(user: User.Data): F[User.Existing]

  def createMany(users: List[User.Data]): F[List[User.Existing]]

  def readOneById(id: UUID): F[Option[User.Existing]]

  def readManyById(ids: List[UUID]): F[List[User.Existing]]

  def readManyByName(name: String): F[List[User.Existing]]

  def readAll: F[List[User.Existing]]

  def updateOne(user: User.Existing): F[User.Existing]

  def updateMany(users: List[User.Existing]): F[List[User.Existing]]

  def deleteOne(user: User.Existing): F[Unit]

  def deleteMany(users: List[User.Existing]): F[Unit]

  def deleteAll: F[Unit]
}

object UserService {
  def of[F[_] : Monad : Clock](userRepo: UserRepository[F]): UserService[F] =
    new UserService[F] {
      override def createOne(user: User.Data): F[User.Existing] =
        createMany(List(user)).map(_.head)

      override def createMany(Users: List[User.Data]): F[List[User.Existing]] =
        writeMany(Users)

      private def writeMany[T <: User](Users: List[T]): F[List[User.Existing]] =
        Clock[F].getZonedDateTimeUTC.flatMap { now =>
          userRepo.writeMany(
            Users.map(User => User.withUpdatedName(User.name.trim, now))
          )
        }

      override def readOneById(id: UUID): F[Option[User.Existing]] =
        readManyById(List(id)).map(_.headOption)

      override def readManyById(ids: List[UUID]): F[List[User.Existing]] =
        userRepo.readManyById(ids)

      override def readManyByName(name: String): F[List[User.Existing]] =
        if (name.isEmpty)
          List.empty.pure[F]
        else
          userRepo.readManyByPartialName(name.trim)

      override val readAll: F[List[User.Existing]] =
        userRepo.readAll

      override def updateOne(user: User.Existing): F[User.Existing] =
        updateMany(List(user)).map(_.head)

      override def updateMany(users: List[User.Existing]): F[List[User.Existing]] =
        writeMany(users)

      override def deleteOne(user: User.Existing): F[Unit] =
        deleteMany(List(user))

      override def deleteMany(users: List[User.Existing]): F[Unit] =
        userRepo.deleteMany(users)

      override val deleteAll: F[Unit] =
        userRepo.deleteAll
    }
}
