package io.tyoras.cards.persistence.user

import io.tyoras.cards.persistence.codecs.skunk.timestampTZ
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import java.time.ZonedDateTime
import java.util.UUID

object Statements:

  object Insert:
    val one: Query[UserDAO.Data, UserDAO.Existing] =
      sql"""INSERT INTO users (name, about)
            VALUES ${UserDAO.Data.codec.values}
            RETURNING *
         """.query(UserDAO.Existing.codec)

    val oneWithId: Query[UUID *: UserDAO.Data *: EmptyTuple, UserDAO.Existing] =
      sql"""INSERT INTO users (id, name, about)
            VALUES(${uuid ~ UserDAO.Data.codec})
            RETURNING *
         """.query(UserDAO.Existing.codec)

    def many(size: Int): Query[List[UserDAO.Data], UserDAO.Existing] =
      sql"""INSERT INTO users (name, about)
            VALUES(${UserDAO.Data.codec.list(size)})
            RETURNING *
         """.query(UserDAO.Existing.codec)

  object Update:
    val one: Query[UserDAO.Existing *: ZonedDateTime *: EmptyTuple, UserDAO.Existing] =
      sql"""UPDATE users
            SET name = ${varchar(100)}, about = $varchar, updated_at = $timestampTZ
            WHERE id = $uuid
            RETURNING *
         """.query(UserDAO.Existing.codec).contramap { case (existing, updatedAt) => (existing.data.name, existing.data.about, updatedAt, existing.id) }

  object Select:
    val all: Query[Void, UserDAO.Existing] =
      sql"""SELECT * FROM users ORDER BY created_at""".query(UserDAO.Existing.codec)

    val byPartialName: Query[String, UserDAO.Existing] =
      sql"""SELECT * FROM users WHERE name ~ ${varchar(100)} ORDER BY created_at""".query(UserDAO.Existing.codec)

    def many(size: Int): Query[List[UUID], UserDAO.Existing] =
      sql"""SELECT * FROM users WHERE id IN (${uuid.list(size)}) ORDER BY created_at""".query(UserDAO.Existing.codec)

    def manyByName(size: Int): Query[List[String], UserDAO.Existing] =
      sql"""SELECT * FROM users WHERE name IN (${varchar(100).list(size)}) ORDER BY created_at""".query(UserDAO.Existing.codec)

  object Delete:
    val all: Command[Void] =
      sql"""DELETE FROM users""".command

    def many(size: Int): Command[List[UUID]] =
      sql"""DELETE FROM users WHERE id in (${uuid.list(size)})""".command
