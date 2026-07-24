package io.tyoras.cards.persistence.user

import cats.effect.{Clock, Resource, Sync}
import cats.syntax.all.*
import io.chrisdavenport.cats.effect.time.implicits.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.user.UserRepository
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.persistence.PersistenceError
import skunk.*

object PostgresUserRepository:
  def of[F[_] : Sync](sessionPool: Resource[F, Session[F]]): F[UserRepository[F]] = Sync[F].delay {
    new UserRepository[F] {
      override def writeMany(users: List[User]): F[List[User.Existing]] = users.traverse {
        case data: User.Data     => insert(data)
        case user: User.Existing => updateOne(user)
      }

      override def insert(data: User.Data, withId: Option[FUUID] = None): F[User.Existing] =
        sessionPool.use { session =>
          val daoData = UserDAO.Data.fromDomain(data)
          withId
            .fold(session.prepareR(Statements.Insert.one).use(_.unique(daoData))) { id =>
              session.prepareR(Statements.Insert.oneWithId).use(_.unique(id -> daoData))
            }
            .flatMap(_.toDomain)
            .adaptErr { case SqlState.UniqueViolation(ex) =>
              PersistenceError("already_exist", "User already exist")
            }
        }

      private def updateOne(user: User.Existing): F[User.Existing] =
        sessionPool.use { session =>
          for
            now     <- Clock[F].getZonedDateTimeUTC
            updated <- session.prepareR(Statements.Update.one).use(_.unique(UserDAO.Existing.fromDomain(user) -> now))
            result  <- Sync[F].fromEither(updated.toDomain)
          yield result
        }

      override def readManyById(ids: List[FUUID]): F[List[User.Existing]] =
        sessionPool.use(_.prepareR(Statements.Select.many(ids.size)).use(_.stream(ids, chunkSize).evalMap(_.toDomain[F]).compile.toList))

      override def readManyByPartialName(name: User.Name): F[List[User.Existing]] =
        sessionPool.use(_.prepareR(Statements.Select.byPartialName).use(_.stream(name, chunkSize).evalMap(_.toDomain[F]).compile.toList))

      override def readManyByName(names: List[User.Name]): F[List[User.Existing]] =
        sessionPool.use(_.prepareR(Statements.Select.manyByName(names.size)).use(_.stream(names, chunkSize).evalMap(_.toDomain[F]).compile.toList))

      override def readAll: F[List[User.Existing]] =
        sessionPool.use(_.prepareR(Statements.Select.all).use(_.stream(Void, chunkSize).evalMap(_.toDomain[F]).compile.toList))

      override def deleteMany(users: List[User.Existing]): F[Unit] =
        sessionPool.use(_.prepareR(Statements.Delete.many(users.size)).use(_.execute(users.map(_.id)).void))

      override def deleteAll: F[Unit] = sessionPool.use(_.execute(Statements.Delete.all).void)
    }
  }

  private val chunkSize = 1024
