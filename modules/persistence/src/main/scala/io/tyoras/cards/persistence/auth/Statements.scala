package io.tyoras.cards.persistence.auth

import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.persistence.codecs.skunk.{fuuid, timestampTZ}
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import java.time.ZonedDateTime

object Statements:
  object Insert:
    val one: Query[UserAuthDAO.Data, UserAuthDAO.Existing] =
      sql"""INSERT INTO user_auth (user_id, password_hash, password_salt)
                  VALUES ${UserAuthDAO.Data.codec.values}
                  RETURNING created_at, updated_at, user_id, password_hash, password_salt
               """.query(UserAuthDAO.Existing.codec)

  object Update:
    val one: Query[UserAuthDAO.Existing *: ZonedDateTime *: EmptyTuple, UserAuthDAO.Existing] =
      sql"""UPDATE user_auth
                  SET password_hash = $bytea, password_salt = $bytea, updated_at = $timestampTZ
                  WHERE user_id = $fuuid
                  RETURNING created_at, updated_at, user_id, password_hash, password_salt
               """.query(UserAuthDAO.Existing.codec).contramap { case (existing, updatedAt) =>
        (existing.data.passwordHash, existing.data.passwordSalt, updatedAt, existing.data.userId)
      }

  object Select:
    val byUserId: Query[FUUID, UserAuthDAO.Existing] =
      sql"""SELECT created_at, updated_at, user_id, password_hash, password_salt
                 FROM user_auth
                 WHERE user_id = $fuuid""".query(UserAuthDAO.Existing.codec)

  object Delete:
    val all: skunk.Command[Void] =
      sql"""DELETE FROM user_auth""".command

    val byUserId: skunk.Command[FUUID] =
      sql"""DELETE FROM user_auth WHERE user_id = $fuuid""".command
