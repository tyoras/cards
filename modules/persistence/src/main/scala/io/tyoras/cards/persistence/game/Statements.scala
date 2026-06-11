package io.tyoras.cards.persistence.game

import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.persistence.codecs.skunk.{fuuid, timestampTZ}
import skunk.*
import skunk.circe.codec.all.*
import skunk.implicits.*
import io.circe.Json

import java.time.ZonedDateTime

object Statements:

  object Insert:
    val one: Query[GameCreationDBModel, GameReadDBModel] =
      sql"""INSERT INTO games (game_type, state, created_by, finished_at)
            VALUES ${GameCreationDBModel.codec.values}
            RETURNING *
         """.query(GameReadDBModel.codec)

    val oneWithId: Query[FUUID ~ GameCreationDBModel, GameReadDBModel] =
      sql"""INSERT INTO games (id, game_type, state, created_by, finished_at)
            VALUES(${fuuid ~ GameCreationDBModel.codec})
            RETURNING *
         """.query(GameReadDBModel.codec)

    val onePlayer: Command[FUUID ~ FUUID] =
      sql"""INSERT INTO gamesplayers (game_id, player_id)
            VALUES(${fuuid ~ fuuid})
           """.command

  object Update:
    val one: Query[GameUpdateDBModel, GameReadDBModel] =
      sql"""UPDATE games
            SET state = $jsonb, updated_at = $timestampTZ
            WHERE id = $fuuid AND updated_at = $timestampTZ
            RETURNING *
         """.query(GameReadDBModel.codec).contramap(e => e.state *: e.updateDate *: e.id *: e.previousUpdate *: EmptyTuple)

  object Select:
    def all(isFinished: Boolean): Query[Void, GameReadDBModel *: FUUID *: EmptyTuple] = {
      val notFinished = sql"g.finished_at IS NULL"
      val finished    = sql"g.finished_at IS NOT NULL"
      sql"""SELECT g.id, g.created_at, g.updated_at, g.game_type, g.state, g.created_by, g.finished_at, p.player_id
           FROM games g INNER JOIN gamesplayers p ON g.id = p.game_id
           WHERE ${if isFinished then finished else notFinished} NULL
           ORDER BY g.created_at
           """.query(GameReadDBModel.codec *: fuuid)
    }

    def many(size: Int): Query[List[FUUID], (GameReadDBModel, FUUID)] =
      sql"""SELECT g.id, g.created_at, g.updated_at, g.game_type, g.state, g.created_by, g.finished_at, p.player_id
            FROM games g INNER JOIN gamesplayers p ON g.id = p.game_id
            WHERE g.id IN (${fuuid.list(size)})
            ORDER BY g.created_at
         """.query(GameReadDBModel.codec *: fuuid)

    def byUser(isFinished: Boolean): Query[FUUID, GameReadDBModel *: FUUID *: EmptyTuple] =
      val notFinished = sql"g.finished_at IS NULL"
      val finished    = sql"g.finished_at IS NOT NULL"
      sql"""SELECT g.id, g.created_at, g.updated_at, g.game_type, g.state, g.created_by, g.finished_at, p.player_id
            FROM games g INNER JOIN gamesplayers p ON g.id = p.game_id
            WHERE g.id IN (
                SELECT game_id
                FROM gamesplayers
                WHERE player_id = $fuuid
            ) AND ${if isFinished then finished else notFinished}
            ORDER BY g.created_at
         """.query(GameReadDBModel.codec *: fuuid)

  object Delete:
    val all: Command[Void] =
      sql"""DELETE FROM games""".command

    def many(size: Int): Command[List[FUUID]] =
      sql"""DELETE FROM games WHERE id in (${fuuid.list(size)})""".command
