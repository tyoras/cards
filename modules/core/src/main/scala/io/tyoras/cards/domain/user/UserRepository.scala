package io.tyoras.cards.domain.user

import io.chrisdavenport.fuuid.FUUID

trait UserRepository[F[_]]:
  def writeMany(users: List[User]): F[List[User.Existing]]

  def insert(data: User.Data, withId: Option[FUUID] = None): F[User.Existing]

  def readManyById(ids: List[FUUID]): F[List[User.Existing]]

  def readManyByPartialName(name: String): F[List[User.Existing]]

  def readAll: F[List[User.Existing]]

  def deleteMany(users: List[User.Existing]): F[Unit]

  def deleteAll: F[Unit]
