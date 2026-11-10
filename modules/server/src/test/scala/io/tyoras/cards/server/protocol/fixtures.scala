package io.tyoras.cards.server.protocol

import cats.data.NonEmptyList
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream
import io.circe.{Codec, Decoder}
import io.tyoras.cards.domain.card.international52Deck
import io.tyoras.cards.domain.game.stats.GameStatService
import io.tyoras.cards.domain.game.stats.model.{PlayerGameStat, PlayerGameStats}
import io.tyoras.cards.domain.game.war.model.*
import io.tyoras.cards.domain.game.{Game, GameService, GameTyp, GameType}
import io.tyoras.cards.domain.user.model.User

import java.time.ZonedDateTime
import java.util.UUID

object fixtures:
  val now: ZonedDateTime = ZonedDateTime.now()

  def newUserId: User.ID = User.ID(UUID.randomUUID())
  def newGameId: Game.ID = Game.ID(UUID.randomUUID())

  def user(id: User.ID, name: String): User.Existing =
    User.Existing(id, now, now, User.Data(User.Name.assume(name), User.About.assume("about")))

  /** Two players context where each player holds half of a 52 cards deck */
  def twoPlayersContext(p1: PlayerId, p2: PlayerId): GameContext =
    val (h1, h2) = international52Deck.splitAt(26)
    GameContext(Map(p1 -> Player(p1, h1), p2 -> Player(p2, h2)), now, Turn.firstTurn)

  /** Context where p2 is already eliminated so the next acknowledgement of the turn finishes the game */
  def almostFinishedState(p1: PlayerId, p2: PlayerId): GameState.PlayerWinTurn =
    val ctx = GameContext(
      Map(p1 -> Player(p1, international52Deck), p2 -> Player(p2, Nil)),
      now,
      Turn.firstTurn,
      eliminations = List(Elimination(p2, Turn.firstTurn))
    )
    GameState.PlayerWinTurn(ctx, p1, Set.empty, Set(p2), acked = Set(p2))

  def warGame(id: Game.ID, state: GameState, finishedAt: Option[ZonedDateTime] = None): Game.Existing[GameState] =
    Game.Existing(
      id,
      now,
      now,
      Game.Data(GameTyp.War, NonEmptyList.fromListUnsafe(state.context.players.keys.toList), state, state.context.players.keys.head, finishedAt)
    )

  /** In memory game service keeping track of the stored games */
  class InMemoryGameService(val store: Ref[IO, Map[Game.ID, Game.Existing[?]]]) extends GameService[IO]:
    override def create[State : Codec](game: Game.Data[State], withId: Option[Game.ID]): IO[Game.Existing[State]] =
      val created = Game.Existing(withId.getOrElse(newGameId), now, now, game)
      store.update(_.updated(created.id, created)).as(created)

    override def update[State : Codec](game: Game.Existing[State]): IO[Game.Existing[State]] =
      store.update(_.updated(game.id, game)).as(game)

    override def finish[State : Codec](gameId: Game.ID, finalState: State, finishedAt: ZonedDateTime): IO[Game.Existing[State]] =
      readById[State](gameId).flatMap {
        case Some(game) => update(game.withUpdatedState(finalState).withFinishedAt(finishedAt))
        case None       => IO.raiseError(new NoSuchElementException(s"game $gameId not found"))
      }

    override def readById[State : Decoder](id: Game.ID): IO[Option[Game.Existing[State]]] =
      store.get.map(_.get(id).map(_.asInstanceOf[Game.Existing[State]]))

    override def readManyByUser[State : Decoder](userId: User.ID, finished: Boolean): IO[List[Game.Existing[State]]] = IO.pure(Nil)
    override def readAll[State : Decoder](finished: Boolean): Stream[IO, Game.Existing[State]]                       = Stream.empty
    override def deleteMany(games: List[Game.Existing[?]]): IO[Unit]                                                 = IO.unit
    override val deleteAll: IO[Unit]                                                                                 = IO.unit

  object InMemoryGameService:
    def make(games: Game.Existing[?]*): IO[InMemoryGameService] =
      Ref.of[IO, Map[Game.ID, Game.Existing[?]]](games.map(g => g.id -> g).toMap).map(new InMemoryGameService(_))

  final case class StatUpdate(game: GameType, winners: List[User.ID], draws: List[User.ID], losers: List[User.ID])

  /** Game stat service recording the stat updates */
  class RecordingGameStatService(val updates: Ref[IO, List[StatUpdate]]) extends GameStatService[IO]:
    override def updatePlayersStats(
        game: GameType,
        winners: List[User.ID],
        draws: List[User.ID],
        losers: List[User.ID]
    ): IO[NonEmptyList[PlayerGameStat.Existing]] =
      val stats = (winners ++ draws ++ losers).map(id => PlayerGameStat.Existing(PlayerGameStat.ID(UUID.randomUUID()), now, now, PlayerGameStat.Data(id, game)))
      updates.update(_ :+ StatUpdate(game, winners, draws, losers)).as(NonEmptyList.fromListUnsafe(stats))

    override def getPlayerStat(playerId: User.ID, game: GameType): IO[PlayerGameStat] = IO.pure(PlayerGameStat.Data(playerId, game))
    override def getPlayerStats(playerId: User.ID): IO[Option[PlayerGameStats]]       = IO.pure(None)
    override def readAll: Stream[IO, PlayerGameStat.Existing]                         = Stream.empty
    override def delete(stat: PlayerGameStat.Existing): IO[Unit]                      = IO.unit
    override def deleteMany(stats: NonEmptyList[PlayerGameStat.Existing]): IO[Unit]   = IO.unit
    override def deleteAll: IO[Unit]                                                  = IO.unit

  object RecordingGameStatService:
    def make: IO[RecordingGameStatService] = Ref.of[IO, List[StatUpdate]](Nil).map(new RecordingGameStatService(_))
