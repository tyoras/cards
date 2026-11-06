package io.tyoras.cards.cli

import cats.Foldable
import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import dev.profunktor.auth.jwt.JwtToken
import io.tyoras.cards.cli.remote.auth.{AuthCredentials, AuthProvider}
import io.tyoras.cards.cli.remote.auth.AuthError.UserNotAuthenticated
import io.tyoras.cards.domain.user.model.User
import org.http4s.client.websocket.{WSClient, WSConnection, WSConnectionHighLevel, WSDataFrame, WSFrame, WSRequest}
import cats.effect.kernel.DeferredSource
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.time.ZonedDateTime
import java.util.UUID

object fixtures:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  val now: ZonedDateTime = ZonedDateTime.parse("2026-01-02T03:04:05Z")

  def userId(n: Int): User.ID = User.ID(UUID.fromString(f"00000000-0000-0000-0000-$n%012d"))

  def user(n: Int, name: String): User.Existing =
    User.Existing(userId(n), now, now, User.Data(User.Name.applyUnsafe(name), User.About.applyUnsafe(s"About $name")))

  val alice: User.Existing = user(1, "Alice")
  val bob: User.Existing   = user(2, "Bob")
  val carol: User.Existing = user(3, "Carol")

  val token: JwtToken = JwtToken("test-token")

  def authProvider(userId: User.ID = alice.id): AuthProvider[IO] = new:
    override def connectedUserCredentials: IO[AuthCredentials] = IO.pure(AuthCredentials(userId, token))

  val unauthenticated: AuthProvider[IO] = new:
    override def connectedUserCredentials: IO[AuthCredentials] = IO.raiseError(UserNotAuthenticated)

  /** Websocket client recording the connection requests and always connecting to the given connection */
  def wsClient(connection: WSConnectionHighLevel[IO], requests: Ref[IO, List[WSRequest]]): WSClient[IO] = new:
    override def connect(request: WSRequest): Resource[IO, WSConnection[IO]] =
      Resource.eval(IO.raiseError(new UnsupportedOperationException("low level connection not supported")))
    override def connectHighLevel(request: WSRequest): Resource[IO, WSConnectionHighLevel[IO]] =
      Resource.eval(requests.update(_ :+ request).as(connection))

  val noWsClient: WSClient[IO] = new:
    override def connect(request: WSRequest): Resource[IO, WSConnection[IO]] =
      Resource.eval(IO.raiseError(new UnsupportedOperationException("no websocket")))
    override def connectHighLevel(request: WSRequest): Resource[IO, WSConnectionHighLevel[IO]] =
      Resource.eval(IO.raiseError(new UnsupportedOperationException("no websocket")))

  /** In-memory websocket connection: records every sent frame and replays the given incoming frames */
  final class FakeWsConnection private (
      incoming: Queue[IO, Option[WSDataFrame]],
      sentRef: Ref[IO, List[WSDataFrame]],
      close: Deferred[IO, WSFrame.Close]
  ) extends WSConnectionHighLevel[IO]:
    def sent: IO[List[WSDataFrame]]        = sentRef.get
    def sentTexts: IO[List[String]]        = sent.map(_.collect { case WSFrame.Text(data, _) => data })
    def push(frame: WSDataFrame): IO[Unit] = incoming.offer(frame.some)
    def end: IO[Unit]                      = incoming.offer(None)

    override def send(wsf: WSDataFrame): IO[Unit]                                  = sentRef.update(_ :+ wsf)
    override def sendMany[G[_] : Foldable, A <: WSDataFrame](wsfs: G[A]): IO[Unit] = wsfs.traverse_(send)
    override def receive: IO[Option[WSDataFrame]]                                  = incoming.take
    override def subprotocol: Option[String]                                       = None
    override def closeFrame: DeferredSource[IO, WSFrame.Close]                     = close

  object FakeWsConnection:
    /** @param terminated whether the incoming stream ends once all the frames have been received */
    def make(frames: List[WSDataFrame], terminated: Boolean = true): IO[FakeWsConnection] =
      for
        queue <- Queue.unbounded[IO, Option[WSDataFrame]]
        _     <- frames.traverse_(f => queue.offer(f.some))
        _     <- queue.offer(None).whenA(terminated)
        sent  <- Ref.of[IO, List[WSDataFrame]](Nil)
        close <- Deferred[IO, WSFrame.Close]
        _     <- close.complete(WSFrame.Close(1000, "bye"))
      yield new FakeWsConnection(queue, sent, close)
