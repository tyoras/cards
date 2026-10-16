package io.tyoras.cards.domain.auth

import cats.effect.Sync
import io.tyoras.cards.domain.auth.model.{Password, UserAuth, UserAuthError}
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.domain.user.model.User
import org.typelevel.log4cats.LoggerFactory
import cats.syntax.all.*
import io.tyoras.cards.util.crypto.Argon2Hasher
import io.github.iltotore.iron.cats.*
import io.tyoras.cards.domain.user.model.User.ID
import io.tyoras.cards.util.error.TechnicalError

trait UserAuthService[F[_]]:
  def createNewUser(userData: User.Data, password: Password): F[User.Existing]
  def changeUserPassword(userId: User.ID, oldPassword: Password, newPassword: Password): F[Unit]

object UserAuthService:
  def of[F[_] : Sync : LoggerFactory](userService: UserService[F], userAuthRepository: UserAuthRepository[F], hasher: Argon2Hasher[F]): UserAuthService[F] =
    new UserAuthService[F]:
      private val logger = LoggerFactory.getLogger

      override def createNewUser(userData: User.Data, password: Password): F[User.Existing] =
        for
          user <- userService.create(userData).onError(logger.error(_)("Failed to create new user data")).adaptErr {
            case e: TechnicalError if e.code == "already_exist" => e
            case _                                              => UserAuthError.UserCreationFailure
          }
          hashedPassword <- hasher.hash(password)
          hash           <- Sync[F].fromValidated(UserAuth.PasswordHashBytes.validated(hashedPassword.hash).leftMap(UserAuthError.InvalidHash(_)))
          salt           <- Sync[F].fromValidated(UserAuth.PasswordSaltBytes.validated(hashedPassword.salt).leftMap(UserAuthError.InvalidSalt(_)))
          userAuth = UserAuth.Data(user.id, hash, salt)
          _ <- userAuthRepository
            .insert(userAuth)
            .onError(logger.error(_)("Failed to persist user credentials"))
            .adaptErr(_ => UserAuthError.CredentialPersistenceFailure)
        yield user

      override def changeUserPassword(userId: ID, oldPassword: Password, newPassword: Password): F[Unit] =
        for
          userAuthRead <- userAuthRepository.readByUserId(userId)
          userAuth     <- Sync[F]
            .fromOption(userAuthRead, UserAuthError.UserNotFound(userId))
            .onError(logger.warn(_)("Failed to change password because user does not exist"))
          oldPasswordHashed <- hasher.hash(oldPassword, userAuth.passwordSalt.some)
          _                 <- UserAuthError.InvalidOldPassword.raiseError.unlessA(oldPasswordHashed.hash.sameElements(userAuth.passwordHash))
          newPasswordHashed <- hasher.hash(newPassword)
          hash              <- Sync[F].fromValidated(UserAuth.PasswordHashBytes.validated(newPasswordHashed.hash).leftMap(UserAuthError.InvalidHash(_)))
          salt              <- Sync[F].fromValidated(UserAuth.PasswordSaltBytes.validated(newPasswordHashed.salt).leftMap(UserAuthError.InvalidSalt(_)))
          _                 <- userAuthRepository.update(userAuth.copy(data = UserAuth.Data(userId, hash, salt)))
        yield ()
