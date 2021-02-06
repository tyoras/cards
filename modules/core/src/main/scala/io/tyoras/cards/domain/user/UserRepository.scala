package io.tyoras.cards.domain.user

import java.util.UUID

trait UserRepository[F[_]] {
  def writeMany(users: List[User]): F[List[User.Existing]]

  def readManyById(ids: List[UUID]): F[List[User.Existing]]

  def readManyByPartialName(name: String): F[List[User.Existing]]

  def readAll: F[List[User.Existing]]

  def deleteMany(users: List[User.Existing]): F[Unit]

  def deleteAll: F[Unit]
}
