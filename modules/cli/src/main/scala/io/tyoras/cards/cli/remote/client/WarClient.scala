package io.tyoras.cards.cli.remote.client

import cats.effect.Async
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import io.tyoras.cards.cli.remote.auth.AuthProvider
import org.http4s.client.websocket.{WSConnectionHighLevel, WSDataFrame, WSFrame}
import fs2.{Pipe, Stream}
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.domain.game.GameType.War
import io.tyoras.cards.shared.protocol.game.Commands.*
import io.circe.syntax.*
import io.tyoras.cards.shared.protocol.game.OutputMessage as ServerMessage
import org.http4s.client.websocket.WSFrame.Text
import io.circe.parser.decode
import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.domain.game.war.model.WarInput
import io.tyoras.cards.domain.game.war.model.WarInput.GameInput.{PlayCard, Ready}
import org.typelevel.log4cats.LoggerFactory

trait WarClient[F[_]]:
  def gameId: FUUID
  def streamServerMessages: Stream[F, ServerMessage]
  def ready: F[Unit]
  def playCard(card: Card): F[Unit]
  def quit: F[Unit]

object WarClient:
  def make[F[_] : Async : LoggerFactory](connectedGameId: FUUID, wsConnection: WSConnectionHighLevel[F], authProvider: AuthProvider[F]): F[WarClient[F]] =
    for
      logger <- LoggerFactory[F].create.map(_.addContext(Map("game_id" -> connectedGameId.toString, "game_type" -> "war")))
      creds  <- authProvider.connectedUserCredentials
      authCommand = AuthCommand(connectedGameId, War, creds.token.value)
      _                <- logger.debug("Sending auth command")
      _                <- wsConnection.send(WSFrame.Text(authCommand.asJson.noSpaces))
      stopStreamSignal <- SignallingRef.of(false)
    yield new WarClient[F]:
      override def gameId: FUUID = connectedGameId

      override def streamServerMessages: Stream[F, ServerMessage] =
        wsConnection.receiveStream.through(decodeServerMessage).interruptWhen(stopStreamSignal)

      override def ready: F[Unit] =
        sendGameInput(Ready(creds.userId))

      override def playCard(card: Card): F[Unit] =
        sendGameInput(PlayCard(creds.userId, card))

      private def sendGameInput(gameInput: WarInput): F[Unit] =
        val command = GameCommand(gameId, War, gameInput.asJson).asJson
        logger.debug(s"Sending game input:\n${command.spaces2}")
        wsConnection.send(WSFrame.Text(command.noSpaces))

      override def quit: F[Unit] =
        stopStreamSignal.set(true) >> wsConnection.closeFrame.get.void

      private val decodeServerMessage: Pipe[F, WSDataFrame, ServerMessage] = _.evalMapFilter {
        case Text(data, _) =>
          Async[F]
            .fromEither(decode[ServerMessage](data))
            .map(_.some)
            .handleErrorWith(logger.warn(_)(s"Failed to decode WS Text frame for game $gameId").as(none))
        case _ => none.pure
      }
