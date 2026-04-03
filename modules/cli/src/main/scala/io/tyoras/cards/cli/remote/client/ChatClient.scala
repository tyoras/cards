package io.tyoras.cards.cli.remote.client

import cats.effect.{Async, Resource}
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import fs2.{Pipe, Stream}
import io.tyoras.cards.cli.remote.auth.AuthProvider
import org.typelevel.log4cats.LoggerFactory
import io.tyoras.cards.shared.protocol.chat.{ChatCommand, OutputMessage as ServerMessage}
import org.http4s.client.websocket.WSFrame.Text
import org.http4s.client.websocket.{WSClient, WSConnectionHighLevel, WSDataFrame, WSFrame, WSRequest}
import io.circe.parser.decode
import io.tyoras.cards.cli.remote.client.ChatClient.ConnectedClient
import io.tyoras.cards.cli.remote.config.CardsClientConfig
import io.tyoras.cards.shared.protocol.chat.ChatCommand.{ChangeRoom, ListRoomMembers, ListRooms}

trait ChatClient[F[_]]:
  def connect: Resource[F, ConnectedClient[F]]

object ChatClient:
  trait ConnectedClient[F[_]]:
    def streamServerMessages: Stream[F, ServerMessage]
    def listRooms: F[Unit]
    def changeRoom(roomName: String): F[Unit]
    def listRoomMembers: F[Unit]
    def chat(text: String): F[Unit]
    def disconnect: F[Unit]

  def make[F[_] : Async : LoggerFactory](config: CardsClientConfig, webSocketClient: WSClient[F], authProvider: AuthProvider[F]): ChatClient[F] =
    new:
      private val chatWsUri = config.wsUri / "ws" / "chat"

      override def connect: Resource[F, ConnectedClient[F]] =
        webSocketClient.connectHighLevel(WSRequest(chatWsUri)).evalMap(make(_, authProvider))

      def make(wsConnection: WSConnectionHighLevel[F], authProvider: AuthProvider[F]): F[ConnectedClient[F]] =
        for
          logger <- LoggerFactory[F].create
          creds  <- authProvider.connectedUserCredentials
          authCommand = ChatCommand.Auth(creds.token)
          _                <- logger.debug("Sending auth command")
          _                <- wsConnection.send(WSFrame.Text(authCommand.asText))
          stopStreamSignal <- SignallingRef.of(false)
        yield new ConnectedClient[F]:
          override def streamServerMessages: Stream[F, ServerMessage] =
            wsConnection.receiveStream.through(decodeServerMessage).interruptWhen(stopStreamSignal)

          override def chat(text: String): F[Unit] = wsConnection.send(WSFrame.Text(text))

          override def listRooms: F[Unit] = sendChatCommand(ListRooms)

          override def changeRoom(roomName: String): F[Unit] = sendChatCommand(ChangeRoom(roomName))

          override def listRoomMembers: F[Unit] = sendChatCommand(ListRoomMembers)

          override def disconnect: F[Unit] =
            stopStreamSignal.set(true) >> wsConnection.closeFrame.get.void

          private def sendChatCommand(command: ChatCommand): F[Unit] =
            val txtCommand = command.asText
            wsConnection.send(WSFrame.Text(txtCommand))

          private val decodeServerMessage: Pipe[F, WSDataFrame, ServerMessage] = _.evalMapFilter {
            case Text(data, _) =>
              Async[F]
                .fromEither(decode[ServerMessage](data))
                .map(_.some)
                .handleErrorWith(logger.warn(_)(s"Failed to decode WS Text frame for chat client").as(none))
            case _ => none.pure
          }
