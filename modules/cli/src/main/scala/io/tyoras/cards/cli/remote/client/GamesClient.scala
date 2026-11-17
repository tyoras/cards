package io.tyoras.cards.cli.remote.client

import cats.data.NonEmptyList
import cats.effect.{Async, Resource}
import io.tyoras.cards.cli.remote.config.CardsClientConfig
import io.tyoras.cards.shared.endpoint.games.Payloads.Response.Game.given
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.Request
import org.http4s.client.Client
import io.scalaland.chimney.dsl.transformInto
import cats.syntax.all.*
import io.scalaland.chimney.Transformer
import io.tyoras.cards.cli.remote.auth.AuthProvider
import io.tyoras.cards.domain.game.war.model.PlayerId
import io.tyoras.cards.domain.game.Game.ID as GameID
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.endpoint
import io.tyoras.cards.shared.endpoint.games.Payloads.Response
import io.tyoras.cards.shared.endpoint.games.war.Payloads as WarPayloads
import io.tyoras.cards.shared.endpoint.games.ers.Payloads as ERsPayloads
import org.http4s.Method.{DELETE, GET, POST}
import org.http4s.client.websocket.{WSClient, WSRequest}
import org.typelevel.log4cats.LoggerFactory

trait GamesClient[F[_]]:
  def listAll: F[List[Game]]
  def findById(gameId: GameID): F[Option[Game]]
  def findByUserId(userId: User.ID, finished: Boolean): F[List[Game]]
  def removeById(gameId: GameID): F[Boolean]
  def createWarGame(players: NonEmptyList[PlayerId]): F[Game]
  def connectWarGame(gameId: GameID): Resource[F, WarClient[F]]
  def createERsGame(players: NonEmptyList[PlayerId]): F[Game]
  def connectERsGame(gameId: GameID): Resource[F, ERsClient[F]]

object GamesClient:
  given Transformer[Response.Game, Game] =
    Transformer.define[Response.Game, Game].enableMethodAccessors.buildTransformer

  def make[F[_] : Async : LoggerFactory](
      config: CardsClientConfig,
      httpClient: Client[F],
      webSocketClient: WSClient[F],
      authProvider: AuthProvider[F]
  ): GamesClient[F] =
    new:
      private val gamesApiUri = config.apiUri / "api" / "games"
      private val warApiUri   = config.apiUri / "api" / "games" / "war"
      private val ersApiUri   = config.apiUri / "api" / "games" / "ers"
      private val warWsUri    = config.wsUri / "ws" / "games" / "war"
      private val ersWsUri    = config.wsUri / "ws" / "games" / "ers"

      override def listAll: F[List[Game]] =
        for
          request <- Request[F](GET, gamesApiUri).pure
          result  <- httpClient.expect[List[Response.Game]](request)
        yield result.map(_.transformInto[Game])

      override def findById(gameId: GameID): F[Option[Game]] =
        for
          request <- Request[F](GET, gamesApiUri / gameId.toString).pure
          result  <- httpClient.expectOption[Response.Game](request)
        yield result.map(_.transformInto[Game])

      override def findByUserId(userId: User.ID, finished: Boolean): F[List[Game]] = {
        val baseUri = gamesApiUri.withQueryParam("user_id", userId.toString)
        val uri     = if finished then baseUri.withQueryParam("finished") else baseUri
        for
          request <- Request[F](GET, uri).pure
          result  <- httpClient.expect[List[Response.Game]](request)
        yield result.map(_.transformInto[Game])
      }

      override def removeById(gameId: GameID): F[Boolean] =
        for
          request <- Request[F](DELETE, gamesApiUri / gameId.toString).pure
          result  <- httpClient.successful(request)
        yield result

      override def createWarGame(players: NonEmptyList[PlayerId]): F[Game] =
        for
          request <- Request[F](POST, warApiUri).withEntity(WarPayloads.Request.Creation(players)).pure
          result  <- httpClient.expect[Response.Game](request)
        yield result.transformInto[Game]

      override def connectWarGame(gameId: GameID): Resource[F, WarClient[F]] =
        webSocketClient.connectHighLevel(WSRequest(warWsUri)).evalMap(WarClient.make(gameId, _, authProvider))

      override def createERsGame(players: NonEmptyList[PlayerId]): F[Game] =
        for
          request <- Request[F](POST, ersApiUri).withEntity(ERsPayloads.Request.Creation(players)).pure
          result  <- httpClient.expect[Response.Game](request)
        yield result.transformInto[Game]

      override def connectERsGame(gameId: GameID): Resource[F, ERsClient[F]] =
        webSocketClient.connectHighLevel(WSRequest(ersWsUri)).evalMap(ERsClient.make(gameId, _, authProvider))
