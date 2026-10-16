package io.tyoras.cards.domain.user

import io.tyoras.cards.domain.user.model.User
import fs2.Stream

trait UserRepository[F[_]]:
  def writeMany(users: List[User]): F[List[User.Existing]]

  def insert(data: User.Data, withId: Option[User.ID] = None): F[User.Existing]

  def readManyById(ids: List[User.ID]): F[List[User.Existing]]

  def readManyByPartialName(name: User.Name): F[List[User.Existing]]

  def readManyByName(names: List[User.Name]): F[List[User.Existing]]

  def readAll: Stream[F, User.Existing]

  def deleteMany(users: List[User.Existing]): F[Unit]

  def deleteAll: F[Unit]
