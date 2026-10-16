package io.tyoras.cards.shared.endpoint.games.war

import cats.data.NonEmptyList
import io.circe.Codec
import io.tyoras.cards.domain.game.war.model.PlayerId

import java.util.UUID
import pl.iterators.kebs.circe.snakified.{flatEncoder, flatDecoder}

object Payloads:
  object Request:
    final case class Creation(players: NonEmptyList[PlayerId]) derives Codec
