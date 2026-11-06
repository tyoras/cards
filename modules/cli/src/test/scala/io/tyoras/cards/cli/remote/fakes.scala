package io.tyoras.cards.cli.remote

import cats.data.NonEmptyList
import cats.effect.{IO, Ref, Resource}
import fs2.Stream
import io.tyoras.cards.cli.fixtures.now
import io.tyoras.cards.cli.remote.client.{ChatClient, Game, GamesClient, UsersClient, WarClient}
import io.tyoras.cards.domain.card.Card
import io.tyoras.cards.domain.game.Game.ID as GameID
import io.tyoras.cards.domain.game.GameType
import io.tyoras.cards.domain.game.war.model.PlayerId
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.protocol.chat.OutputMessage as ChatMessage
import io.tyoras.cards.shared.protocol.game.OutputMessage as GameMessage

import java.time.ZonedDateTime
import java.util.UUID

object fakes:
  def gameId(n: Int): GameID = GameID(UUID.fromString(f"10000000-0000-0000-0000-$n%012d"))

  def game(n: Int, gameType: GameType, players: NonEmptyList[User.ID], updatedAt: ZonedDateTime = now): Game =
    Game(gameId(n), now, updatedAt, gameType, players)

  class FakeUsersClient(users: IO[List[User.Existing]]) extends UsersClient[IO]:
    override def listAll: IO[List[User.Existing]]                         = users
    override def findById(userId: User.ID): IO[Option[User.Existing]]     = users.map(_.find(_.id == userId))
    override def findByPartialName(name: String): IO[List[User.Existing]] = users.map(_.filter(_.name.contains(name)))

  class FakeGamesClient(
      games: IO[List[Game]] = IO.pure(Nil),
      created: Ref[IO, List[NonEmptyList[PlayerId]]],
      warClient: Resource[IO, WarClient[IO]] = Resource.eval(IO.raiseError(new UnsupportedOperationException("no war client")))
  ) extends GamesClient[IO]:
    override def listAll: IO[List[Game]]                                          = games
    override def findById(gameId: GameID): IO[Option[Game]]                       = games.map(_.find(_.id == gameId))
    override def findByUserId(userId: User.ID, finished: Boolean): IO[List[Game]] = games.map(_.filter(_.players.toList.contains(userId)))
    override def removeById(gameId: GameID): IO[Boolean]                          = IO.pure(true)
    override def createWarGame(players: NonEmptyList[PlayerId]): IO[Game]         =
      created.update(_ :+ players).as(Game(gameId(99), now, now, io.tyoras.cards.domain.game.GameTyp.War, players))
    override def connectWarGame(gameId: GameID): Resource[IO, WarClient[IO]] = warClient

  /** War client recording the calls made to it */
  class FakeWarClient(val gameId: GameID, messages: List[GameMessage], calls: Ref[IO, List[String]], failing: Boolean = false) extends WarClient[IO]:
    private def record(call: String): IO[Unit]                 = if failing then IO.raiseError(new Exception(s"$call failed")) else calls.update(_ :+ call)
    override def streamServerMessages: Stream[IO, GameMessage] = Stream.emits(messages)
    override def ready: IO[Unit]                               = record("ready")
    override def playCard(card: Card): IO[Unit]                = record(s"play ${card.id}")
    override def getState: IO[Unit]                            = record("state")
    override def quit: IO[Unit]                                = record("quit")

  /** Connected chat client recording the calls made to it */
  class FakeChatClient(messages: List[ChatMessage], calls: Ref[IO, List[String]], failing: Boolean = false) extends ChatClient.ConnectedClient[IO]:
    private def record(call: String): IO[Unit]                 = if failing then IO.raiseError(new Exception(s"$call failed")) else calls.update(_ :+ call)
    override def streamServerMessages: Stream[IO, ChatMessage] = Stream.emits(messages)
    override def listRooms: IO[Unit]                           = record("rooms")
    override def changeRoom(roomName: String): IO[Unit]        = record(s"room $roomName")
    override def listRoomMembers: IO[Unit]                     = record("members")
    override def chat(text: String): IO[Unit]                  = record(s"chat $text")
    override def disconnect: IO[Unit]                          = record("disconnect")
