package io.tyoras.cards.persistence.game.stats

import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.persistence.codecs.skunk.{fuuid, gameType, timestampTZ}
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import java.time.ZonedDateTime

import io.tyoras.cards.domain.game.GameType

object Statements:

  object Insert:
    val one: Query[GameStatDAO.Data, GameStatDAO.Existing] =
      sql"""INSERT INTO games_stats (player_id, game_type, won, draw, lost)
            VALUES ${GameStatDAO.Data.codec.values}
            RETURNING *
         """.query(GameStatDAO.Existing.codec)

    val oneWithId: Query[FUUID *: GameStatDAO.Data *: EmptyTuple, GameStatDAO.Existing] =
      sql"""INSERT INTO games_stats (id, player_id, game_type, won, draw, lost)
            VALUES(${fuuid ~ GameStatDAO.Data.codec})
            RETURNING *
         """.query(GameStatDAO.Existing.codec)

    def many(size: Int): Query[List[GameStatDAO.Data], GameStatDAO.Existing] =
      sql"""INSERT INTO games_stats (player_id, game_type, won, draw, lost)
            VALUES(${GameStatDAO.Data.codec.list(size)})
            RETURNING *
         """.query(GameStatDAO.Existing.codec)

  object Update:
    val one: Query[GameStatDAO.Existing *: ZonedDateTime *: EmptyTuple, GameStatDAO.Existing] =
      sql"""UPDATE games_stats
            SET player_id = $fuuid, game_type = $gameType, won = $int4, draw = $int4, lost = $int4, updated_at = $timestampTZ
            WHERE id = $fuuid
            RETURNING *
         """.query(GameStatDAO.Existing.codec).contramap { case (existing, updatedAt) =>
        (existing.data.playerId, existing.data.game, existing.data.won, existing.data.draw, existing.data.lost, updatedAt, existing.id)
      }

  object Select:
    val all: Query[Void, GameStatDAO.Existing] =
      sql"""SELECT * FROM games_stats ORDER BY created_at""".query(GameStatDAO.Existing.codec)

    val one: Query[FUUID *: GameType *: EmptyTuple, GameStatDAO.Existing] =
      sql"""SELECT * FROM games_stats WHERE player_id = $fuuid AND game_type = $gameType""".query(GameStatDAO.Existing.codec)

    def many(size: Int): Query[List[FUUID], GameStatDAO.Existing] =
      sql"""SELECT * FROM games_stats WHERE id IN (${fuuid.list(size)}) ORDER BY created_at""".query(GameStatDAO.Existing.codec)

    def manyByPlayerId(size: Int): Query[List[FUUID], GameStatDAO.Existing] =
      sql"""SELECT * FROM games_stats WHERE player_id IN (${fuuid.list(size)}) ORDER BY created_at""".query(GameStatDAO.Existing.codec)

  object Delete:
    val all: Command[Void] =
      sql"""DELETE FROM games_stats""".command

    def many(size: Int): Command[List[FUUID]] =
      sql"""DELETE FROM games_stats WHERE id in (${fuuid.list(size)})""".command
