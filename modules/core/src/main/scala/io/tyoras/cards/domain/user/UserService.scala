package io.tyoras.cards.domain.user

import cats.Monad
import cats.effect.Clock
import cats.syntax.all._
import io.chrisdavenport.cats.effect.time.implicits.ClockOps
import io.chrisdavenport.fuuid.FUUID

trait UserService[F[_]] {
  def create(user: User.Data): F[User.Existing]

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

  def createOrUpdate(id: FUUID, user: User.Data): F[User.Existing]
}

object UserService {
  def of[F[_] : Monad : Clock](userRepo: UserRepository[F]): UserService[F] =
    new UserService[F] {
      override def create(user: User.Data): F[User.Existing] =
        createMany(List(user)).map(_.head)

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
        if (name.isEmpty)
          List.empty.pure[F]
        else
          userRepo.readManyByPartialName(name.trim)

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

      override def createOrUpdate(id: FUUID, user: User.Data): F[User.Existing] = for {
        found   <- readById(id)
        now     <- Clock[F].getZonedDateTimeUTC
        updated <- found.fold(create(user))(existing => update(existing.withUpdatedName(user.name, now).withUpdatedAbout(user.about, now)))
      } yield updated
    }
}
