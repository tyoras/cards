package io.tyoras.cards.cli.local.game.war

import cats.data.NonEmptyList
import cats.effect.*
import WarCliError.{InvalidInput, InvalidState}
import io.tyoras.cards.domain.game.schnapsen.PlayerId
import io.tyoras.cards.domain.game.war.model.GameState.*
import io.tyoras.cards.domain.game.war.model.WarInput.MetaInput.*
import io.tyoras.cards.domain.game.war.model.*
import io.tyoras.cards.domain.game.war.*
import io.tyoras.cards.util.logging.syntax.*
import org.typelevel.log4cats.LoggerFactory
import cats.syntax.all.*
import io.tyoras.cards.domain.game.war.model.WarInput.GameInput.*

trait InputParser[F[_]]:
  def parse(state: GameState, playerId: PlayerId, rawInput: String): F[NonEmptyList[WarInput]]

object InputParser:
  def apply[F[_] : Sync : LoggerFactory]: InputParser[F] =
    val logger = LoggerFactory.getLogger

    (state: GameState, playerId: PlayerId, rawInput: String) =>
      logger.debug(playerId.ctx(playerIdKey))(s"Input [${if rawInput.isEmpty then "❌" else rawInput}]") >>
        (rawInput match
          case "\\q" => NonEmptyList.one(End(playerId)).pure
          case "\\r" => NonEmptyList.one(Restart(playerId)).pure
          case _ =>
            state match
              case s: Init          => Sync[F].fromOption(NonEmptyList.fromList(s.notReady.toList.map(Ready(_))), InvalidState)
              case s: BattleTurn    => Sync[F].fromOption(s.pickFirstCard(playerId), InvalidState).map(_.id).map(PlayCard(playerId, _)).map(NonEmptyList.one)
              case s: WarTurn       => Sync[F].fromOption(s.pickFirstCard(playerId), InvalidState).map(_.id).map(PlayCard(playerId, _)).map(NonEmptyList.one)
              case s: PlayerWinTurn => Sync[F].fromOption(NonEmptyList.fromList(s.notAcked.toList.map(Ready(_))), InvalidState)
              // In finish state we only expect Quit or Restart inputs
              case _: Finish => logger.warn("Unexpected input in Finish state") *> InvalidInput.raiseError
              // Exit state is handled exclusively by the main game loop
              case _: Exit => logger.warn("Unexpected input in Exit state") *> InvalidState.raiseError)
