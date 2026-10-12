package io.tyoras.cards.domain.auth

import io.tyoras.cards.domain.auth.model.UserAuth
import io.tyoras.cards.domain.user.model.User

trait UserAuthRepository[F[_]]:
  def insert(userAuth: UserAuth.Data): F[UserAuth.Existing]

  def readByUserId(userId: User.ID): F[Option[UserAuth.Existing]]

  def update(userAuth: UserAuth.Existing): F[UserAuth.Existing]

  def delete(userAuth: UserAuth.Existing): F[Unit]

  def deleteAll: F[Unit]
