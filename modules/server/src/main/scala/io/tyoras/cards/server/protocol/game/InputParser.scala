package io.tyoras.cards.server.protocol.game

import cats.effect.{Ref, Sync}
import cats.syntax.all.*
import dev.profunktor.auth.jwt.JwtToken
import io.circe.Error
import io.tyoras.cards.shared.protocol.game.Commands.*
import io.tyoras.cards.shared.protocol.game.OutputMessage
import io.tyoras.cards.shared.protocol.game.OutputMessage.*
import org.typelevel.log4cats.LoggerFactory

trait InputParser[F[_]]:
  def parse(playerRef: Ref[F, Option[ConnectedPlayer]], text: String): F[List[OutputMessage]]

object InputParser:
  def make[F[_] : Sync : LoggerFactory](protocol: GameProtocol[F]): InputParser[F] = new InputParser[F]:
    private val logger = LoggerFactory.getLogger
    override def parse(playerRef: Ref[F, Option[ConnectedPlayer]], text: String): F[List[OutputMessage]] =
      text.trim match
        case "" => List(DiscardMessage).pure
        case txt =>
          playerRef.get.flatMap {
            _.fold(processUnauthenticatedCommand(txt, playerRef)) { player =>
              processAuthenticatedCommand(player, txt)
            }
          }

    private def processUnauthenticatedCommand(text: String, playerRef: Ref[F, Option[ConnectedPlayer]]): F[List[OutputMessage]] =
      (for
        authCmd    <- Sync[F].fromEither(io.circe.parser.decode[AuthCommand](text))
        authResult <- protocol.auth(authCmd.gameId, authCmd.gameType, JwtToken(authCmd.jwt)).map(List(_))
        _          <- handleAuthResult(authResult, playerRef)
      yield authResult).handleError { case e: Error =>
        List(UnsupportedCommand)
      }

    private def handleAuthResult(authResult: List[OutputMessage], playerRef: Ref[F, Option[ConnectedPlayer]]): F[Unit] =
      val connected = authResult.collectFirst { case PlayerConnectionSuccess(gameId, id, name) =>
        ConnectedPlayer(gameId, id, name)
      }
      val logMsg =
        connected.fold(s"WebSocket authentication attempt failed")(c => s"Player ${c.name} [id=${c.playerId}] successfully connected to game ${c.gameId}")
      logger.info(logMsg) *> playerRef.set(connected)

    private def processAuthenticatedCommand(player: ConnectedPlayer, text: String): F[List[OutputMessage]] =
      (for
        gameCmd <- Sync[F].fromEither(io.circe.parser.decode[GameCommand](text))
        input   <- Sync[F].fromEither(gameCmd.input.as[gameCmd.gameType.Input](using gameCmd.gameType.given_Decoder_Input))
        result  <- protocol.submitInput(gameCmd.gameId, gameCmd.gameType, player.playerId, input)(using gameCmd.gameType.given_Encoder_State)
      yield result).handleError { case e: Error =>
        List(ProtocolError(player.gameId, player.playerId, "invalid_input", e.getMessage))
      }
