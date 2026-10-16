package io.tyoras.cards.persistence.game

import cats.Eq
import cats.data.NonEmptyList
import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.tyoras.cards.domain.game.Game.Existing
import io.tyoras.cards.domain.game.*
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.persistence.codecs.skunk.{gameType, timestampTZ}
import skunk.Codec
import skunk.circe.codec.all.*
import skunk.codec.all.*

import java.time.ZonedDateTime
import java.util.UUID

final case class GameCreationDBModel(game: GameType, state: Json, createdBy: UUID, finishedAt: Option[ZonedDateTime])
object GameCreationDBModel:
  val codec: Codec[GameCreationDBModel] = (gameType *: jsonb *: uuid *: timestampTZ.opt).to[GameCreationDBModel]

  def fromGameData[State : Encoder](data: Game.Data[State]): GameCreationDBModel =
    GameCreationDBModel(data.gameType, data.state.asJson, data.createdBy.unwrap, data.finishedAt)

final case class GameUpdateDBModel(state: Json, updateDate: ZonedDateTime, finishedAt: Option[ZonedDateTime], id: UUID, previousUpdate: ZonedDateTime)
object GameUpdateDBModel:
  def fromExisingGame[State : Encoder](existing: Existing[State], updateDate: ZonedDateTime): GameUpdateDBModel =
    GameUpdateDBModel(existing.data.state.asJson, updateDate, existing.data.finishedAt, existing.id.unwrap, existing.updatedAt)

final case class GameReadDBModel(id: UUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: GameCreationDBModel):
  def toExistingGame[State : Decoder](players: NonEmptyList[UUID]): Either[DecodingFailure, Game.Existing[State]] =
    data.state
      .as[State]
      .map(state =>
        Game.Existing(
          Game.ID(id),
          createdAt,
          updatedAt,
          Game.Data[State](data.game.asInstanceOf[GameTyp[State, ?]], players.map(User.ID(_)), state, User.ID(data.createdBy), data.finishedAt)
        )
      )

object GameReadDBModel:
  given Eq[GameReadDBModel] = Eq.fromUniversalEquals

  val codec: Codec[GameReadDBModel] = (uuid *: timestampTZ *: timestampTZ *: GameCreationDBModel.codec).to[GameReadDBModel]
