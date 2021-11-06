package io.tyoras.cards.persistence.user

import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.user.User
import io.tyoras.cards.persistence.{fuuid, timestampTZ}
import skunk._
import skunk.codec.all._
import skunk.implicits._

import java.time.ZonedDateTime

object Statements:
  extension (data: User.Data.type) def codec: Codec[User.Data] = (varchar(100) ~ varchar).gimap[User.Data]

  extension (user: User.Existing.type) def codec: Codec[User.Existing] = (fuuid ~ timestampTZ ~ timestampTZ ~ User.Data.codec).gimap[User.Existing]

  object Insert:
    val one: Query[User.Data, User.Existing] =
      sql"""INSERT INTO users (name, about)
            VALUES(${User.Data.codec})
            RETURNING *
         """.query(User.Existing.codec)

    val oneWithId: Query[FUUID ~ User.Data, User.Existing] =
      sql"""INSERT INTO users (id, name, about)
            VALUES(${fuuid ~ User.Data.codec})
            RETURNING *
         """.query(User.Existing.codec)

    def many(size: Int): Query[List[User.Data], User.Existing] =
      sql"""INSERT INTO users (name, about)
            VALUES(${User.Data.codec.list(size)})
            RETURNING *
         """.query(User.Existing.codec)

  object Update:
    val one: Query[User.Existing ~ ZonedDateTime, User.Existing] =
      sql"""UPDATE users
            SET name = ${varchar(100)}, about = $varchar, updated_at = $timestampTZ
            WHERE id = $fuuid
            RETURNING *
         """.query(User.Existing.codec).contramap(input)

    private def input(e: User.Existing ~ ZonedDateTime): String ~ String ~ ZonedDateTime ~ FUUID =
      val (data, updatedAt) = e
      data.name ~ data.about ~ updatedAt ~ data.id

  object Select:
    val all: Query[Void, User.Existing] =
      sql"""SELECT * FROM users""".query(User.Existing.codec)

    val byName: Query[String, User.Existing] =
      sql"""SELECT * FROM users WHERE name ~ ${varchar(100)}""".query(User.Existing.codec)

    def many(size: Int): Query[List[FUUID], User.Existing] =
      sql"""SELECT * FROM users WHERE id in (${fuuid.list(size)})""".query(User.Existing.codec)

  object Delete:
    val all: Command[Void] =
      sql"""DELETE FROM users""".command

    def many(size: Int): Command[List[FUUID]] =
      sql"""DELETE FROM users WHERE id in (${fuuid.list(size)})""".command
