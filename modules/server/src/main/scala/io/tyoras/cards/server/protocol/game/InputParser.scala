package io.tyoras.cards.server.protocol.game

import cats.effect.{Ref, Sync}
import cats.syntax.all.*
import dev.profunktor.auth.jwt.JwtToken
import io.circe.Error
import io.tyoras.cards.shared.protocol.game.Commands.*
import io.tyoras.cards.shared.protocol.game.Commands.AuthenticatedCommand.*
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
              processAuthenticatedCommand(player, txt, playerRef)
            }
          }

    private def processUnauthenticatedCommand(text: String, playerRef: Ref[F, Option[ConnectedPlayer]]): F[List[OutputMessage]] =
      (for
        authCmd    <- Sync[F].fromEither(io.circe.parser.decode[AuthCommand](text))
        authResult <- protocol.authPlayer(authCmd.gameId, authCmd.gameType, JwtToken(authCmd.jwt)).map(List(_))
        _          <- handleAuthResult(authResult, playerRef)
      yield authResult).handleError { case e: Error =>
        List(UnsupportedCommand)
      }

    private def handleAuthResult(authResult: List[OutputMessage], playerRef: Ref[F, Option[ConnectedPlayer]]): F[Unit] =
      val connected = authResult.collectFirst { case PlayerConnectionSuccess(gameId, gameType, id, name) =>
        ConnectedPlayer(gameId, gameType, id, name)
      }
      val logMsg =
        connected.fold(s"WebSocket authentication attempt failed")(c => s"Player ${c.name} [id=${c.playerId}] successfully connected to game ${c.gameId}")
      logger.info(logMsg) *> playerRef.set(connected)

    private def processAuthenticatedCommand(player: ConnectedPlayer, text: String, playerRef: Ref[F, Option[ConnectedPlayer]]): F[List[OutputMessage]] =
      (for
        cmd <- Sync[F].fromEither(io.circe.parser.decode[AuthenticatedCommand](text))
        result <- cmd match
          case gameCmd: GameCommand =>
            for
              input  <- Sync[F].fromEither(gameCmd.input.as[player.gameType.Input](using player.gameType.given_Decoder_Input))
              result <- protocol.submitInput(gameCmd.gameId, player.gameType, player.playerId, input)(using player.gameType.given_Encoder_State)
            yield result
          case stateCmd: StateCommand =>
            protocol.currentState(stateCmd.gameId, player.gameType, player.playerId)(using player.gameType.given_Encoder_State).map(List(_))
          case quitCmd: QuitCommand => protocol.disconnect(playerRef).map(List(_))
      yield result).handleError { case e: Error =>
        List(ProtocolError(player.gameId, player.playerId, "invalid_input", e.getMessage))
      }
