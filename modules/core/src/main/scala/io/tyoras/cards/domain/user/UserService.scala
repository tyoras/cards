package io.tyoras.cards.domain.user

import cats.Monad
import cats.syntax.all.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.user.model.User

trait UserService[F[_]]:
  def create(user: User.Data, withId: Option[FUUID] = None): F[User.Existing]

  def createMany(users: List[User.Data]): F[List[User.Existing]]

  def readById(id: FUUID): F[Option[User.Existing]]

  def readByName(name: User.Name): F[Option[User.Existing]]

  def readManyById(ids: List[FUUID]): F[List[User.Existing]]

  def readManyByPartialName(name: User.Name): F[List[User.Existing]]

  def readAll: F[List[User.Existing]]

  def update(user: User.Existing): F[User.Existing]

  def updateMany(users: List[User.Existing]): F[List[User.Existing]]

  def delete(user: User.Existing): F[Unit]

  def deleteMany(users: List[User.Existing]): F[Unit]

  def deleteAll: F[Unit]

object UserService:
  def of[F[_] : Monad](userRepo: UserRepository[F]): UserService[F] = new:
    override def create(user: User.Data, withId: Option[FUUID]): F[User.Existing] =
      userRepo.insert(user, withId)

    override def createMany(users: List[User.Data]): F[List[User.Existing]] =
      writeMany(users)

    private def writeMany[U <: User](users: List[U]): F[List[User.Existing]] =
      userRepo.writeMany(users)

    override def readById(id: FUUID): F[Option[User.Existing]] =
      readManyById(List(id)).map(_.headOption)

    override def readByName(name: User.Name): F[Option[User.Existing]] =
      userRepo.readManyByName(List(name)).map(_.headOption)

    override def readManyById(ids: List[FUUID]): F[List[User.Existing]] =
      userRepo.readManyById(ids)

    override def readManyByPartialName(name: User.Name): F[List[User.Existing]] =
      userRepo.readManyByPartialName(name)

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
