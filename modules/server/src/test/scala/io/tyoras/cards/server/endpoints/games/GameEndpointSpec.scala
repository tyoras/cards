package io.tyoras.cards.server.endpoints.games

import cats.effect.IO
import cats.syntax.all.*
import io.tyoras.cards.domain.game.ers.model.ERsGameState
import io.tyoras.cards.domain.game.war.model.WarGameState
import io.tyoras.cards.server.endpoints.EndpointSpec
import io.tyoras.cards.server.endpoints.utils.{TestData, TestRoutes}
import io.tyoras.cards.server.protocol.fixtures.*
import io.tyoras.cards.shared.endpoint.ErrorPayloads.Response.ApiMessage
import io.tyoras.cards.shared.endpoint.games.Payloads
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.{HttpRoutes, Status}
import pl.iterators.baklava.EmptyBody
import sttp.model.Method.{DELETE, GET}

import java.util.UUID

class GameEndpointSpec extends EndpointSpec:
  /** The authenticated user */
  private val creator = TestData.existingUser
  private val other   = newUserId
  private val third   = newUserId

  /** Ongoing War game created by the authenticated user */
  private val ongoingGame = warGame(newGameId, WarGameState.Init(twoPlayersContext(creator.id, other)))

  /** Finished Egyptian Ratscrew game created by the authenticated user */
  private val finishedGame = ersGame(newGameId, ERsGameState.Init(ersTwoPlayersContext(creator.id, other)), finishedAt = now.some)

  /** Ongoing War game between other players, created by one of them */
  private val othersGame = warGame(newGameId, WarGameState.Init(twoPlayersContext(other, third)))

  private val unknownGameId = newGameId

  private case class Env(routes: HttpRoutes[IO], games: InMemoryGameService)

  private val makeEnv: IO[Env] =
    for
      games    <- InMemoryGameService.make(ongoingGame, finishedGame, othersGame)
      endpoint <- GameEndpoint.of[IO](games)
    yield Env(TestRoutes.of(endpoint, creator), games)

  /** Run a test against a fresh environment */
  private def withEnv[A](test: Env => A): A = test(makeEnv.unsafeRunSync())

  override lazy val testedRoutes: HttpRoutes[IO] = makeEnv.unsafeRunSync().routes

  private def isStored(env: Env, game: UUID): Boolean = env.games.store.get.unsafeRunSync().keySet.exists(_.unwrap == game)

  path(path = "/api/games")(
    supports(
      GET,
      queryParameters = (
        q[Option[UUID]]("user_id", "Only list the games of this player"),
        q[Option[Boolean]]("finished", "Flag: when present, list the finished games instead of the ongoing ones")
      ),
      description = "List the ongoing or finished games, optionally only those of a given player",
      summary = "List games",
      securitySchemes = Seq(bearerScheme),
      tags = Seq("games")
    )(
      onRequest(queryParameters = (Option.empty[UUID], Option.empty[Boolean]))
        .respondsWith[List[Payloads.Response.Game]](Status.Ok, description = "Return all the ongoing games")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.Ok.code
            response.body.map(_.id) should contain theSameElementsAs List(ongoingGame.id, othersGame.id)
          }
        },
      onRequest(queryParameters = (Option.empty[UUID], true.some))
        .respondsWith[List[Payloads.Response.Game]](Status.Ok, description = "Return all the finished games")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.Ok.code
            response.body.map(_.id) shouldBe List(finishedGame.id)
          }
        },
      onRequest(queryParameters = (third.unwrap.some, Option.empty[Boolean]))
        .respondsWith[List[Payloads.Response.Game]](Status.Ok, description = "Return the ongoing games of the given player")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.Ok.code
            response.body.map(_.id) shouldBe List(othersGame.id)
          }
        }
    )
  )

  path(path = "/api/games/{id}")(
    supports(
      GET,
      pathParameters = p[UUID]("id", "Game ID"),
      description = "Get a game by its ID, whatever its type",
      summary = "Get a game",
      securitySchemes = Seq(bearerScheme),
      tags = Seq("games")
    )(
      onRequest(pathParameters = ongoingGame.id.unwrap).respondsWith[Payloads.Response.Game](Status.Ok, description = "Return the game").assert { ctx =>
        withEnv { e =>
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.Ok.code
          response.body shouldBe Payloads.Response.Game.fromExistingGame(ongoingGame)
        }
      },
      onRequest(pathParameters = unknownGameId.unwrap)
        .respondsWith[ApiMessage](Status.NotFound, description = "Return 404 when the game does not exist")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.NotFound.code
            response.body.code shouldBe "not_found"
          }
        }
    ),
    supports(
      DELETE,
      pathParameters = p[UUID]("id", "Game ID"),
      description = "Delete a game created by the authenticated user",
      summary = "Delete a game",
      securitySchemes = Seq(bearerScheme),
      tags = Seq("games")
    )(
      onRequest(pathParameters = ongoingGame.id.unwrap).respondsWith[EmptyBody](Status.NoContent, description = "The game has been deleted").assert { ctx =>
        withEnv { e =>
          ctx.performRequest(e.routes).status.code shouldBe Status.NoContent.code
          isStored(e, ongoingGame.id.unwrap) shouldBe false
        }
      },
      onRequest(pathParameters = othersGame.id.unwrap)
        .respondsWith[EmptyBody](Status.Forbidden, description = "Return 403 when the game was created by another user")
        .assert { ctx =>
          withEnv { e =>
            ctx.performRequest(e.routes).status.code shouldBe Status.Forbidden.code
            isStored(e, othersGame.id.unwrap) shouldBe true
          }
        },
      onRequest(pathParameters = unknownGameId.unwrap)
        .respondsWith[ApiMessage](Status.NotFound, description = "Return 404 when the game does not exist")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.NotFound.code
            response.body.code shouldBe "not_found"
          }
        }
    )
  )
