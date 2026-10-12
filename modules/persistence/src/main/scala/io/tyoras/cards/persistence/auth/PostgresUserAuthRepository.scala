package io.tyoras.cards.persistence.auth

import cats.effect.{Clock, Resource, Sync}
import cats.syntax.all.*
import io.chrisdavenport.cats.effect.time.implicits.*
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.auth.UserAuthRepository
import io.tyoras.cards.domain.auth.model.UserAuth
import io.tyoras.cards.persistence.PersistenceError
import skunk.*
import org.typelevel.log4cats.LoggerFactory

object PostgresUserAuthRepository:
  def of[F[_] : Sync : LoggerFactory](sessionPool: Resource[F, Session[F]]): F[UserAuthRepository[F]] = Sync[F].delay {
    new UserAuthRepository[F] {

      private val logger = LoggerFactory.getLogger

      override def insert(data: UserAuth.Data): F[UserAuth.Existing] =
        sessionPool.use { session =>
          val daoData = UserAuthDAO.Data.fromDomain(data)
          session
            .prepareR(Statements.Insert.one)
            .use(_.unique(daoData))
            .flatMap(_.toDomainF)
            .onError(e => logger.error(e)(s"Failed to insert auth for user ${data.userId}"))
            .adaptErr {
              case SqlState.UniqueViolation(ex) =>
                PersistenceError("already_exist", "User auth already exist")
              case e =>
                PersistenceError("unexpected_error", "Failed to insert user auth")
            }
        }

      override def update(userAuth: UserAuth.Existing): F[UserAuth.Existing] =
        sessionPool.use { session =>
          for
            now     <- Clock[F].getZonedDateTimeUTC
            updated <- session
              .prepareR(Statements.Update.one)
              .use(_.unique(UserAuthDAO.Existing.fromDomain(userAuth) -> now))
              .onError(logger.error(_)(s"Failed to update auth for user ${userAuth.userId}"))
              .adaptError(_ => PersistenceError("unexpected_error", "Failed to update user auth"))
            result <- Sync[F].fromEither(updated.toDomain)
          yield result
        }

      override def readByUserId(userId: FUUID): F[Option[UserAuth.Existing]] =
        sessionPool.use {
          _.prepareR(Statements.Select.byUserId)
            .use(_.option(userId))
            .flatMap(_.traverse(_.toDomainF))
            .onError(logger.error(_)(s"Failed to read auth for user $userId"))
            .adaptErr(_ => PersistenceError("unexpected_error", "Failed to read user auth by user id"))
        }

      override def delete(userAuth: UserAuth.Existing): F[Unit] =
        sessionPool.use(
          _.prepareR(Statements.Delete.byUserId)
            .use(_.execute(userAuth.userId))
            .onError(logger.error(_)(s"Failed to delete auth for user ${userAuth.userId}"))
            .adaptErr(_ => PersistenceError("unexpected_error", "Failed to delete user auth by user id"))
            .void
        )

      override def deleteAll: F[Unit] =
        sessionPool
          .use(_.execute(Statements.Delete.all).void)
          .onError(logger.error(_)(s"Failed to delete all user auth"))
          .adaptErr(_ => PersistenceError("unexpected_error", "Failed to delete all user auth"))
    }
  }
