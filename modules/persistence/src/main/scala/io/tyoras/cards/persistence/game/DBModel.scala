package io.tyoras.cards.persistence.game

import cats.Eq
import cats.data.NonEmptyList
import io.chrisdavenport.fuuid.FUUID
import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.tyoras.cards.domain.game.Game.Existing
import io.tyoras.cards.domain.game.{Game, GameType}
import io.tyoras.cards.persistence.{fuuid, gameType, timestampTZ}
import skunk.Codec
import skunk.circe.codec.all.*
import skunk.codec.all.*
import skunk.implicits.*

import java.time.ZonedDateTime

final case class GameCreationDBModel(game: GameType, state: Json)
object GameCreationDBModel:
  val codec: Codec[GameCreationDBModel] = (gameType ~ jsonb).gimap[GameCreationDBModel]

  def fromGameData[State : Encoder](data: Game.Data[State]): GameCreationDBModel = GameCreationDBModel(data.gameType, data.state.asJson)

final case class GameUpdateDBModel(state: Json, updateDate: ZonedDateTime, id: FUUID, previousUpdate: ZonedDateTime)
object GameUpdateDBModel:
  def fromExisingGame[State : Encoder](existing: Existing[State], updateDate: ZonedDateTime): GameUpdateDBModel =
    GameUpdateDBModel(existing.data.state.asJson, updateDate, existing.id, existing.updatedAt)

final case class GameReadDBModel(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: GameCreationDBModel):
  def toExistingGame[State : Decoder](players: NonEmptyList[FUUID]): Either[DecodingFailure, Game.Existing[State]] =
    data.state.as[State].map(state => Game.Existing(id, createdAt, updatedAt, Game.Data[State](data.game, players, state)))

object GameReadDBModel:
  given Eq[GameReadDBModel] = Eq.fromUniversalEquals

  val codec: Codec[GameReadDBModel] = (fuuid ~ timestampTZ ~ timestampTZ ~ GameCreationDBModel.codec).gimap[GameReadDBModel]
