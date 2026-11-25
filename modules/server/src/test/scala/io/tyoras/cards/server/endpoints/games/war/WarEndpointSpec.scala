package io.tyoras.cards.server.endpoints.games.war

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import io.tyoras.cards.domain.game.war.War
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.domain.game.war.model.WarGameState
import io.tyoras.cards.domain.game.{Game, GameService, GameTyp}
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.server.endpoints.Endpoint
import io.tyoras.cards.server.endpoints.games.GameTypeEndpointSpec
import io.tyoras.cards.server.protocol.chat.ChatProtocol
import io.tyoras.cards.server.protocol.fixtures.*
import io.tyoras.cards.server.protocol.game.GameProtocol
import io.tyoras.cards.shared.endpoint.ErrorPayloads.Response.ApiMessage
import io.tyoras.cards.shared.endpoint.games.Payloads
import io.tyoras.cards.shared.endpoint.games.war.Payloads.Request.Creation
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.Status
import sttp.model.Method.{GET, POST}

import java.util.UUID

class WarEndpointSpec extends GameTypeEndpointSpec:
  override protected def makeEndpoint(
      games: GameService[IO],
      users: UserService[IO],
      protocol: GameProtocol[IO],
      chat: ChatProtocol[IO]
  ): Resource[IO, Endpoint[IO]] = WarEndpoint.make[IO](games, users, protocol, chat)

  /** Register an active War game between the given players */
  override protected def registerGame(e: Env, p1: User.ID, p2: User.ID): IO[Game.ID] =
    val gameId = newGameId
    val state  = WarGameState.Init(twoPlayersContext(p1, p2))
    for
      _   <- e.games.update(warGame(gameId, state))
      war <- War.fromState[IO](gameId, state)
      _   <- e.protocol.registerActiveGame(gameId, war)
    yield gameId

  path(path = "/api/games/war")(
    supports(
      POST,
      pathParameters = (),
      description = "Create a new War game",
      summary = "Create a new War game",
      securitySchemes = Seq(bearerScheme),
      tags = Seq("games", "war")
    )(
      onRequest(body = Creation(NonEmptyList.of(creator.id, other.id)))
        .respondsWith[Payloads.Response.Game](Status.Created, description = "Return the created game")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.Created.code
            val game = response.body
            game.gameType shouldBe GameTyp.War
            game.players shouldBe NonEmptyList.of(creator.id, other.id)

            val stored = e.games.readById[WarGameState](game.id).unsafeRunSync()
            stored.map(_.data.createdBy) shouldBe Some(creator.id)
            stored.map(_.state) match
              case Some(init: WarGameState.Init) =>
                init.ready shouldBe empty
                init.context.players.keySet shouldBe Set(creator.id, other.id)
              case s => fail(s"Expected a stored Init state but got $s")

            e.protocol.activeGames.unsafeRunSync().get(game.id, GameTyp.War) shouldBe defined
          }
        },
      onRequest(body = Creation(NonEmptyList.of(creator.id, newUserId)))
        .respondsWith[ApiMessage](Status.BadRequest, description = "Return 400 when some players are unknown")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.BadRequest.code
            response.body.code shouldBe "invalid_request"
            e.games.store.get.unsafeRunSync() shouldBe empty
          }
        }
    )
  )

  path(path = "/api/games/war/active")(
    supports(
      GET,
      pathParameters = (),
      description = "List the active War games of the authenticated player",
      summary = "List the player active War games",
      securitySchemes = Seq(bearerScheme),
      tags = Seq("games", "war")
    )(
      onRequest().respondsWith[List[UUID]](Status.Ok, description = "Return the IDs of the player active games").assert { ctx =>
        withEnv { e =>
          val playerGame = registerGame(e, creator.id, other.id).unsafeRunSync()
          registerGame(e, other.id, third.id).unsafeRunSync() // not involving the player, must not be listed

          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.Ok.code
          response.body shouldBe List(playerGame.unwrap)
        }
      }
    )
  )
