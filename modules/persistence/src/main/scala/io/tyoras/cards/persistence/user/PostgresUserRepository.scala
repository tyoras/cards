package io.tyoras.cards.persistence.user

import cats.effect.{Clock, Resource, Sync}
import cats.syntax.all.*
import io.chrisdavenport.cats.effect.time.implicits.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.user.UserRepository
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.persistence.PersistenceError
import skunk.*
import fs2.Stream
import org.typelevel.log4cats.LoggerFactory

object PostgresUserRepository:
  def of[F[_] : Sync : LoggerFactory](sessionPool: Resource[F, Session[F]]): F[UserRepository[F]] = Sync[F].delay {
    new UserRepository[F] {

      private val logger = LoggerFactory.getLogger

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
            .flatMap(_.toDomainF)
            .onError(e => logger.error(e)(s"Failed to insert user ${withId.getOrElse("no id provided")}"))
            .adaptErr {
              case SqlState.UniqueViolation(ex) =>
                PersistenceError("already_exist", "User already exist")
              case e =>
                PersistenceError("unexpected_error", "Failed to insert user")
            }
        }

      private def updateOne(user: User.Existing): F[User.Existing] =
        sessionPool.use { session =>
          for
            now     <- Clock[F].getZonedDateTimeUTC
            updated <- session
              .prepareR(Statements.Update.one)
              .use(_.unique(UserDAO.Existing.fromDomain(user) -> now))
              .onError(logger.error(_)(s"Failed to update user ${user.id}"))
              .adaptError(_ => PersistenceError("unexpected_error", "Failed to update user"))
            result <- Sync[F].fromEither(updated.toDomain)
          yield result
        }

      override def readManyById(ids: List[FUUID]): F[List[User.Existing]] =
        sessionPool
          .use(_.prepareR(Statements.Select.many(ids.size)).use(_.stream(ids, chunkSize).evalMap(_.toDomainF[F]).compile.toList))
          .onError(logger.error(_)("Failed to read many user by their ids"))
          .adaptErr(_ => PersistenceError("unexpected_error", "Failed to read users by id"))

      override def readManyByPartialName(name: User.Name): F[List[User.Existing]] =
        sessionPool
          .use(_.prepareR(Statements.Select.byPartialName).use(_.stream(name, chunkSize).evalMap(_.toDomainF[F]).compile.toList))
          .onError(logger.error(_)(s"Failed to read many user by partial name $name"))
          .adaptErr(_ => PersistenceError("unexpected_error", "Failed to read users by partial name"))

      override def readManyByName(names: List[User.Name]): F[List[User.Existing]] =
        sessionPool
          .use(_.prepareR(Statements.Select.manyByName(names.size)).use(_.stream(names, chunkSize).evalMap(_.toDomainF[F]).compile.toList))
          .onError(logger.error(_)(s"Failed to read many user by their names $names"))
          .adaptErr(_ => PersistenceError("unexpected_error", "Failed to read users by name"))

      override def readAll: Stream[F, User.Existing] = {
        for
          session  <- Stream.resource(sessionPool)
          prepared <- Stream.resource(session.prepareR(Statements.Select.all))
          results  <- prepared.stream(Void, chunkSize).evalMap(_.toDomainF[F])
        yield results
      }.onError(e => Stream.eval(logger.error(e)("Failed to read all users"))).adaptErr(_ => PersistenceError("unexpected_error", "Failed to read all users"))

      override def deleteMany(users: List[User.Existing]): F[Unit] =
        sessionPool
          .use(_.prepareR(Statements.Delete.many(users.size)).use(_.execute(users.map(_.id)).void))
          .onError(logger.error(_)(s"Failed to delete many users ${users.map(_.id)}"))
          .adaptErr(_ => PersistenceError("unexpected_error", "Failed to delete users"))
          .whenA(users.nonEmpty)

      override def deleteAll: F[Unit] = sessionPool
        .use(_.execute(Statements.Delete.all).void)
        .onError(logger.error(_)("Failed to delete all users"))
        .adaptErr(_ => PersistenceError("unexpected_error", "Failed to delete all users"))
    }
  }

  private val chunkSize = 1024
