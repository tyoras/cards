package io.tyoras.cards.persistence.user

import cats.effect.{Clock, Resource, Sync}
import cats.syntax.all._
import io.chrisdavenport.cats.effect.time.implicits._
import io.tyoras.cards.domain.user.{User, UserRepository}
import skunk.Session

import java.util.UUID

object PostgresUserRepository {
  def of[F[_] : Sync](sessionPool: Resource[F, Session[F]]): F[UserRepository[F]] = F.delay {
    new UserRepository[F] {
      override def writeMany(users: List[User]): F[List[User.Existing]] = users.traverse {
        case data: User.Data => insertOne(data)
        case user: User.Existing => updateOne(user)
      }

      private def insertOne(data: User.Data): F[User.Existing] =
        sessionPool.use { _.prepare(Statements.Insert.one).use(_.unique(data)) }

      private def updateOne(user: User.Existing): F[User.Existing] =
        sessionPool.use { session =>
          for {
            now     <- Clock[F].getZonedDateTimeUTC
            updated <- session.prepare(Statements.Update.one).use(_.unique(user -> now))
          } yield updated
        }

      override def readManyById(ids: List[UUID]): F[List[User.Existing]] =
        sessionPool.use(_.prepare(Statements.Select.many(ids.size)).use(_.stream(ids.to(List), 1024).compile.toList))

      override def readManyByPartialName(name: String): F[List[User.Existing]] =
        sessionPool.use(_.prepare(Statements.Select.byName).use(_.stream(name, 1024).compile.toList))

      override def readAll: F[List[User.Existing]] =
        sessionPool.use(_.execute(Statements.Select.all).map(_.to(List)))

      override def deleteMany(users: List[User.Existing]): F[Unit] =
        sessionPool.use(_.prepare(Statements.Delete.many(users.size)).use(_.execute(users.to(List).map(_.id)).void))

      override def deleteAll: F[Unit] = sessionPool.use(_.execute(Statements.Delete.all).void)
    }
  }

}
