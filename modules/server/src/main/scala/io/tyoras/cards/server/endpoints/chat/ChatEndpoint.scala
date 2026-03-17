package io.tyoras.cards.server.endpoints.chat

import cats.effect.implicits.*
import cats.effect.kernel.Ref
import cats.effect.{Concurrent, Resource, Temporal}
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.concurrent.Topic
import fs2.io.file.Files
import fs2.{Pipe, Stream}
import io.tyoras.cards.server.endpoints.Endpoint
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.{HttpRoutes, MediaType, StaticFile}
import io.circe.syntax.*
import io.tyoras.cards.server.protocol.chat.OutputMessage.{ChatMsg, DiscardMessage, KeepAlive, SendToUser}
import io.tyoras.cards.server.protocol.chat.{ChatProtocol, ChatUser, InputParser, OutputMessage}
import org.http4s.headers.`Content-Type`

import scala.concurrent.duration.DurationInt

object ChatEndpoint:
  def make[F[_] : Files : Concurrent : Temporal](protocol: ChatProtocol[F]): Resource[F, Endpoint[F]] =
    for
      // needed to order the messages as FIFO
      messageQueue <- Resource.eval(Queue.unbounded[F, OutputMessage])
      // publishing/subscription
      messageTopic <- Resource.eval(Topic[F, OutputMessage])
      _ <- Stream
        .fromQueueUnterminated(messageQueue)
        .through(messageTopic.publish)
        .concurrently(
          // heartbeat to prevent network timeout
          Stream.awakeEvery[F](30.seconds).as(KeepAlive).through(messageTopic.publish)
        )
        .compile
        .drain
        .background
      inputParser = InputParser.make(protocol)
    yield new Endpoint[F] with Http4sDsl[F] {
      override val routes: HttpRoutes[F] =
        HttpRoutes.of[F] {
          case request @ GET -> Root / "chat.html" =>
            StaticFile
              .fromPath(
                fs2.io.file.Path(getClass.getClassLoader.getResource("chat.html").getFile),
                Some(request)
              )
              .getOrElseF(NotFound())
          case GET -> Root / "chat" / "metrics" =>
            def currentState = protocol.currentState.map { state =>
              s"""
                 |<!Doctype html>
                 |<title>Chat Server State</title>
                 |<body>
                 |<pre>Users: ${state.roomsByUser.keys.size}</pre>
                 |<pre>Rooms: ${state.roomMembers.keys.size}</pre>
                 |<pre>Overview:
                 |${state.roomMembers.keys.toList
                  .map(room => state.roomMembers.getOrElse(room, Set()).map(_.name).toList.sorted.mkString(s"${room.room} Room Members:\n\t", "\n\t", ""))
                  .mkString("Rooms:\n\t", "\n\t", "")}
                 |</pre>
                 |</body>
                 |</html>
            """.stripMargin
            }

            currentState.flatMap(Ok(_, `Content-Type`(MediaType.text.html)))
        }

      override val wsRoutes: WebSocketBuilder2[F] => HttpRoutes[F] = wsBuilder =>
        HttpRoutes.of[F] { case GET -> Root / "chat" =>
          for
            userRef               <- Ref.of[F, Option[ChatUser]](None)
            unregisteredUserQueue <- Queue.unbounded[F, OutputMessage]
            ws                    <- wsBuilder.build(wsSend(unregisteredUserQueue, userRef), wsReceive(userRef, unregisteredUserQueue))
          yield ws
        }

      // out
      private def wsSend(unregisteredUserQueue: Queue[F, OutputMessage], userRef: Ref[F, Option[ChatUser]]): Stream[F, WebSocketFrame] =
        def unregisteredStream = Stream
          .fromQueueUnterminated(unregisteredUserQueue)
          .filter {
            case DiscardMessage => false
            case _              => true
          }
          .map(processMsg)

        def mainStream = messageTopic.subscribe(maxQueued = 1000).evalFilter(filterMsg(_, userRef)).map(processMsg)

        Stream(unregisteredStream, mainStream).parJoinUnbounded

      private def filterMsg(msg: OutputMessage, userRef: Ref[F, Option[ChatUser]]): F[Boolean] =
        msg match
          case DiscardMessage => false.pure
          case m: SendToUser  => userRef.get.map(_.fold(false)(m.forUser))
          case m: ChatMsg     => userRef.get.map(_.fold(false)(m.forUser))
          case _              => true.pure

      private def processMsg(msg: OutputMessage): WebSocketFrame =
        msg match
          case KeepAlive => WebSocketFrame.Ping()
          case _         => WebSocketFrame.Text(msg.asJson.noSpaces)

      // in & out
      private def wsReceive(userRef: Ref[F, Option[ChatUser]], unregisteredUserQueue: Queue[F, OutputMessage]): Pipe[F, WebSocketFrame, Unit] =
        handleWebSocketStream(_, userRef)
          .evalMap { m =>
            userRef.get.flatMap {
              case Some(_) => messageQueue.offer(m)
              case None    => unregisteredUserQueue.offer(m)
            }
          }
          .concurrently {
            Stream.awakeEvery(30.seconds).as(KeepAlive).foreach(unregisteredUserQueue.offer)
          }

      private def handleWebSocketStream(frameStream: Stream[F, WebSocketFrame], userRef: Ref[F, Option[ChatUser]]): Stream[F, OutputMessage] =
        for
          frame <- frameStream
          outputMessage <- Stream.evalSeq(
            frame match
              case WebSocketFrame.Text(text, _) => inputParser.parse(userRef, text)
              // at the moment there is a known bug in ember-server that prevent the stream to know about the close frame (see https://github.com/http4s/http4s/issues/6806)
              case WebSocketFrame.Close(_) => protocol.disconnect(userRef)
          )
        yield outputMessage
    }
