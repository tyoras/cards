package io.tyoras.cards.persistence.user

import cats.effect.{Clock, Resource, Sync}
import cats.syntax.all._
import io.chrisdavenport.cats.effect.time.implicits._
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.user.{User, UserRepository}
import skunk.Session

object PostgresUserRepository {
  def of[F[_] : Sync](sessionPool: Resource[F, Session[F]]): F[UserRepository[F]] = Sync[F].delay {
    new UserRepository[F] {
      override def writeMany(users: List[User]): F[List[User.Existing]] = users.traverse {
        case data: User.Data => insert(data)
        case user: User.Existing => updateOne(user)
      }

      override def insert(data: User.Data, withId: Option[FUUID] = None): F[User.Existing] =
        sessionPool.use { session =>
          withId.fold(session.prepare(Statements.Insert.one).use(_.unique(data))) { id =>
            session.prepare(Statements.Insert.oneWithId).use(_.unique(id -> data))
          }
        }

      private def updateOne(user: User.Existing): F[User.Existing] =
        sessionPool.use { session =>
          for {
            now     <- Clock[F].getZonedDateTimeUTC
            updated <- session.prepare(Statements.Update.one).use(_.unique(user -> now))
          } yield updated
        }

      override def readManyById(ids: List[FUUID]): F[List[User.Existing]] =
        sessionPool.use(_.prepare(Statements.Select.many(ids.size)).use(_.stream(ids, chunkSize).compile.toList))

      override def readManyByPartialName(name: String): F[List[User.Existing]] =
        sessionPool.use(_.prepare(Statements.Select.byName).use(_.stream(name, chunkSize).compile.toList))

      override def readAll: F[List[User.Existing]] =
        sessionPool.use(_.execute(Statements.Select.all))

      override def deleteMany(users: List[User.Existing]): F[Unit] =
        sessionPool.use(_.prepare(Statements.Delete.many(users.size)).use(_.execute(users.map(_.id)).void))

      override def deleteAll: F[Unit] = sessionPool.use(_.execute(Statements.Delete.all).void)
    }
  }

  private val chunkSize = 1024

}
