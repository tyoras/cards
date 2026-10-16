package io.tyoras.cards.persistence.game

import io.tyoras.cards.persistence.codecs.skunk.timestampTZ
import skunk.*
import skunk.codec.all.*
import skunk.circe.codec.json.jsonb
import skunk.implicits.*
import io.circe.Json

import java.time.ZonedDateTime
import java.util.UUID

object Statements:

  object Insert:
    val one: Query[GameCreationDBModel, GameReadDBModel] =
      sql"""INSERT INTO games (game_type, state, created_by, finished_at)
            VALUES ${GameCreationDBModel.codec.values}
            RETURNING *
         """.query(GameReadDBModel.codec)

    val oneWithId: Query[UUID ~ GameCreationDBModel, GameReadDBModel] =
      sql"""INSERT INTO games (id, game_type, state, created_by, finished_at)
            VALUES(${uuid ~ GameCreationDBModel.codec})
            RETURNING *
         """.query(GameReadDBModel.codec)

    val onePlayer: Command[UUID ~ UUID] =
      sql"""INSERT INTO gamesplayers (game_id, player_id)
            VALUES(${uuid ~ uuid})
           """.command

  object Update:
    val one: Query[GameUpdateDBModel, GameReadDBModel] =
      sql"""UPDATE games
            SET state = $jsonb, updated_at = $timestampTZ, finished_at = ${timestampTZ.opt}
            WHERE id = $uuid AND updated_at = $timestampTZ
            RETURNING *
         """.query(GameReadDBModel.codec).contramap(e => e.state *: e.updateDate *: e.finishedAt *: e.id *: e.previousUpdate *: EmptyTuple)

  object Select:
    def all(isFinished: Boolean): Query[Void, GameReadDBModel *: UUID *: EmptyTuple] = {
      val notFinished = sql"g.finished_at IS NULL"
      val finished    = sql"g.finished_at IS NOT NULL"
      sql"""SELECT g.id, g.created_at, g.updated_at, g.game_type, g.state, g.created_by, g.finished_at, p.player_id
           FROM games g INNER JOIN gamesplayers p ON g.id = p.game_id
           WHERE ${if isFinished then finished else notFinished}
           ORDER BY g.created_at
           """.query(GameReadDBModel.codec *: uuid)
    }

    def many(size: Int): Query[List[UUID], (GameReadDBModel, UUID)] =
      sql"""SELECT g.id, g.created_at, g.updated_at, g.game_type, g.state, g.created_by, g.finished_at, p.player_id
            FROM games g INNER JOIN gamesplayers p ON g.id = p.game_id
            WHERE g.id IN (${uuid.list(size)})
            ORDER BY g.created_at
         """.query(GameReadDBModel.codec *: uuid)

    def byUser(isFinished: Boolean): Query[UUID, GameReadDBModel *: UUID *: EmptyTuple] =
      val notFinished = sql"g.finished_at IS NULL"
      val finished    = sql"g.finished_at IS NOT NULL"
      sql"""SELECT g.id, g.created_at, g.updated_at, g.game_type, g.state, g.created_by, g.finished_at, p.player_id
            FROM games g INNER JOIN gamesplayers p ON g.id = p.game_id
            WHERE g.id IN (
                SELECT game_id
                FROM gamesplayers
                WHERE player_id = $uuid
            ) AND ${if isFinished then finished else notFinished}
            ORDER BY g.created_at
         """.query(GameReadDBModel.codec *: uuid)

  object Delete:
    val all: Command[Void] =
      sql"""DELETE FROM games""".command

    def many(size: Int): Command[List[UUID]] =
      sql"""DELETE FROM games WHERE id in (${uuid.list(size)})""".command
