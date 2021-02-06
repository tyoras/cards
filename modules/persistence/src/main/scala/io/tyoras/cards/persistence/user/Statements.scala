package io.tyoras.cards.persistence.user

import io.tyoras.cards.domain.user.User
import io.tyoras.cards.persistence.timestampTZ
import skunk._
import skunk.codec.all._
import skunk.implicits._

import java.time.ZonedDateTime
import java.util.UUID

object Statements {
  final implicit private class UserDataOps(private val data: User.Data.type) {
    val codec: Codec[User.Data] = (varchar(100) ~ varchar).gimap[User.Data]
  }

  final implicit private class UserExistingOps(private val user: User.Existing.type) {
    val codec: Codec[User.Existing] = (uuid ~ timestampTZ ~ timestampTZ ~ User.Data.codec).gimap[User.Existing]
  }

  object Insert {
    val one: Query[User.Data, User.Existing] =
      sql"""INSERT INTO users (name, about)
            VALUES(${User.Data.codec})
            RETURNING *
         """.query(User.Existing.codec)

    def many(size: Int): Query[List[User.Data], User.Existing] =
      sql"""INSERT INTO users (name, about)
            VALUES(${User.Data.codec.list(size)})
            RETURNING *
         """.query(User.Existing.codec)
  }

  object Update {
    val one: Query[User.Existing ~ ZonedDateTime, User.Existing] =
      sql"""UPDATE users
            SET name = ${varchar(100)}, about = $varchar, updated_at = $timestampTZ
            WHERE id = $uuid
            RETURNING *
         """.query(User.Existing.codec).contramap(input)

    private def input(e: User.Existing ~ ZonedDateTime): String ~ String ~ ZonedDateTime ~ UUID = {
      val (data, updatedAt) = e
      data.name ~ data.about ~ updatedAt ~ data.id
    }
  }

  object Select {
    val all: Query[Void, User.Existing] =
      sql"""SELECT * FROM users""".query(User.Existing.codec)

    val byName: Query[String, User.Existing] =
      sql"""SELECT * FROM users WHERE name ~ ${varchar(100)}""".query(User.Existing.codec)

    def many(size: Int): Query[List[UUID], User.Existing] =
      sql"""SELECT * FROM users WHERE id in (${uuid.list(size)})""".query(User.Existing.codec)
  }

  object Delete {
    val all: Command[Void] =
      sql"""DELETE FROM users""".command

    def many(size: Int): Command[List[UUID]] =
      sql"""DELETE FROM users WHERE id in (${uuid.list(size)})""".command
  }
}
