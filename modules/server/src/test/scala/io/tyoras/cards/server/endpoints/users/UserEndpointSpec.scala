package io.tyoras.cards.server.endpoints.users

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.tyoras.cards.domain.auth.UserAuthService
import io.tyoras.cards.domain.auth.model.{Password, UserAuthError}
import io.tyoras.cards.domain.game.GameTyp
import io.tyoras.cards.domain.game.stats.model.{PlayerGameStat, PlayerGameStats}
import io.tyoras.cards.domain.user.UserService
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.server.endpoints.EndpointSpec
import io.tyoras.cards.server.endpoints.utils.{InMemoryUserService, TestData, TestRoutes}
import io.tyoras.cards.server.protocol.fixtures.*
import io.tyoras.cards.shared.endpoint.ErrorPayloads.Response.ApiMessage
import io.tyoras.cards.shared.endpoint.users.Payloads
import io.tyoras.cards.shared.endpoint.users.Payloads.Request.{Creation, PasswordChange}
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.{HttpRoutes, Status}
import pl.iterators.baklava.EmptyBody
import sttp.model.Method.{DELETE, GET, PATCH, POST, PUT}

import java.util.UUID

class UserEndpointSpec extends EndpointSpec:
  /** The authenticated user */
  private val creator   = TestData.existingUser
  private val other     = user(newUserId, "other")
  private val unknownId = newUserId

  private val creatorStats = PlayerGameStat.Existing(PlayerGameStat.ID(UUID.randomUUID()), now, now, PlayerGameStat.Data(creator.id, GameTyp.War, 3, 1, 2))

  /** User auth service delegating the user creation to the user service and only accepting the test user password as old password */
  private class TestUserAuthService(users: UserService[IO]) extends UserAuthService[IO]:
    override def createNewUser(userData: User.Data, password: Password): IO[User.Existing] = users.create(userData)

    override def changeUserPassword(userId: User.ID, oldPassword: Password, newPassword: Password): IO[Unit] =
      users.readById(userId).flatMap {
        case None                                                    => UserAuthError.UserNotFound(userId).raiseError
        case Some(_) if oldPassword != TestData.existingUserPassword => UserAuthError.InvalidOldPassword.raiseError
        case Some(_)                                                 => IO.unit
      }

  private case class Env(routes: HttpRoutes[IO], users: InMemoryUserService)

  private val makeEnv: IO[Env] =
    for
      users   <- InMemoryUserService.make(creator, other)
      updates <- RecordingGameStatService.make.map(_.updates)
      stats = new RecordingGameStatService(updates):
        override def getPlayerStats(playerId: User.ID): IO[Option[PlayerGameStats]] =
          IO.pure(Option.when(playerId == creator.id)(PlayerGameStats(creator.id, NonEmptyList.one(creatorStats))))
      endpoint <- UserEndpoint.of[IO](users, new TestUserAuthService(users), stats)
    yield Env(TestRoutes.of(endpoint, creator), users)

  /** Run a test against a fresh environment */
  private def withEnv[A](test: Env => A): A = test(makeEnv.unsafeRunSync())

  override lazy val testedRoutes: HttpRoutes[IO] = makeEnv.unsafeRunSync().routes

  private def stored(env: Env, id: User.ID): Option[User.Existing] = env.users.readById(id).unsafeRunSync()

  path(path = "/api/users")(
    supports(
      POST,
      description = "Register a new user with its credentials",
      summary = "Create a new user",
      tags = Seq("users")
    )(
      onRequest(body = Creation(name = "newbie".some, password = "password123".some, about = "Just arrived".some))
        .respondsWith[Payloads.Response.User](Status.Created, description = "Return the created user")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.Created.code
            response.body.name shouldBe "newbie"
            response.body.about shouldBe "Just arrived"
            stored(e, response.body.id).map(_.name: String) shouldBe Some("newbie")
          }
        },
      onRequest(body = Creation(name = None, password = "short".some, about = "Just arrived".some))
        .respondsWith[ApiMessage](Status.UnprocessableContent, description = "Return 422 when the payload is invalid")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.UnprocessableContent.code
            (response.body.errors.map(_.field) should contain).allOf("name", "password")
            e.users.store.get.unsafeRunSync().size shouldBe 2
          }
        }
    ),
    supports(
      GET,
      queryParameters = q[Option[String]]("name", "Only list the users whose name contains this value"),
      description = "List the users, optionally filtered by name",
      summary = "List users",
      tags = Seq("users")
    )(
      onRequest(queryParameters = Option.empty[String]).respondsWith[List[Payloads.Response.User]](Status.Ok, description = "Return all the users").assert {
        ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.Ok.code
            response.body.map(_.id) should contain theSameElementsAs List(creator.id, other.id)
          }
      },
      onRequest(queryParameters = "OTH".some)
        .respondsWith[List[Payloads.Response.User]](Status.Ok, description = "Return the users whose name contains the given value, ignoring case")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.Ok.code
            response.body.map(_.id) shouldBe List(other.id)
          }
        }
    )
  )

  path(path = "/api/users/{id}")(
    supports(
      GET,
      pathParameters = p[UUID]("id", "User ID"),
      description = "Get a user by its ID",
      summary = "Get a user",
      tags = Seq("users")
    )(
      onRequest(pathParameters = other.id.unwrap).respondsWith[Payloads.Response.User](Status.Ok, description = "Return the user").assert { ctx =>
        withEnv { e =>
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.Ok.code
          response.body.id shouldBe other.id
          response.body.name shouldBe "other"
        }
      },
      onRequest(pathParameters = unknownId.unwrap).respondsWith[ApiMessage](Status.NotFound, description = "Return 404 when the user does not exist").assert {
        ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.NotFound.code
            response.body.code shouldBe "not_found"
          }
      }
    ),
    supports(
      PUT,
      pathParameters = p[UUID]("id", "User ID, must be the authenticated user"),
      description = "Update the authenticated user, or create it if it does not exist yet",
      summary = "Update a user",
      securitySchemes = Seq(bearerScheme),
      tags = Seq("users")
    )(
      onRequest(body = Creation(name = "renamed".some, password = None, about = "New description".some), pathParameters = creator.id.unwrap)
        .respondsWith[Payloads.Response.User](Status.Ok, description = "Return the updated user")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.Ok.code
            response.body.name shouldBe "renamed"
            stored(e, creator.id).map(u => (u.name: String, u.about: String)) shouldBe Some(("renamed", "New description"))
          }
        },
      onRequest(body = Creation(name = None, password = None, about = None), pathParameters = creator.id.unwrap)
        .respondsWith[ApiMessage](Status.UnprocessableContent, description = "Return 422 when the payload is invalid")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.UnprocessableContent.code
            (response.body.errors.map(_.field) should contain).allOf("name", "about")
            stored(e, creator.id) shouldBe Some(creator)
          }
        },
      onRequest(body = Creation(name = "renamed".some, password = None, about = "New description".some), pathParameters = other.id.unwrap)
        .respondsWith[EmptyBody](Status.Forbidden, description = "Return 403 when updating another user")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.Forbidden.code
            stored(e, other.id) shouldBe Some(other)
          }
        }
    ),
    supports(
      DELETE,
      pathParameters = p[UUID]("id", "User ID, must be the authenticated user"),
      description = "Delete the authenticated user",
      summary = "Delete a user",
      securitySchemes = Seq(bearerScheme),
      tags = Seq("users")
    )(
      onRequest(pathParameters = creator.id.unwrap).respondsWith[EmptyBody](Status.NoContent, description = "The user has been deleted").assert { ctx =>
        withEnv { e =>
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.NoContent.code
          stored(e, creator.id) shouldBe None
        }
      },
      onRequest(pathParameters = other.id.unwrap).respondsWith[EmptyBody](Status.Forbidden, description = "Return 403 when deleting another user").assert { ctx =>
        withEnv { e =>
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.Forbidden.code
          stored(e, other.id) shouldBe Some(other)
        }
      }
    )
  )

  path(path = "/api/users/{id}/password")(
    supports(
      PATCH,
      pathParameters = p[UUID]("id", "User ID, must be the authenticated user"),
      description = "Change the password of the authenticated user",
      summary = "Change a user password",
      securitySchemes = Seq(bearerScheme),
      tags = Seq("users")
    )(
      onRequest(
        body = PasswordChange(oldPassword = (TestData.existingUserPassword: String).some, newPassword = "new_password".some),
        pathParameters = creator.id.unwrap
      ).respondsWith[EmptyBody](Status.NoContent, description = "The password has been changed").assert { ctx =>
        withEnv { e =>
          ctx.performRequest(e.routes).status.code shouldBe Status.NoContent.code
        }
      },
      onRequest(body = PasswordChange(oldPassword = "wrong_password".some, newPassword = "new_password".some), pathParameters = creator.id.unwrap)
        .respondsWith[ApiMessage](Status.BadRequest, description = "Return 400 when the old password is not the current one")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.BadRequest.code
            response.body.code shouldBe "invalid_old_password"
          }
        },
      onRequest(
        body = PasswordChange(oldPassword = (TestData.existingUserPassword: String).some, newPassword = "short".some),
        pathParameters = creator.id.unwrap
      ).respondsWith[ApiMessage](Status.UnprocessableContent, description = "Return 422 when the new password is invalid").assert { ctx =>
        withEnv { e =>
          val response = ctx.performRequest(e.routes)

          response.status.code shouldBe Status.UnprocessableContent.code
          response.body.errors.map(_.field) should contain("new_password")
        }
      },
      onRequest(body = PasswordChange(oldPassword = "password".some, newPassword = "new_password".some), pathParameters = other.id.unwrap)
        .respondsWith[EmptyBody](Status.Forbidden, description = "Return 403 when changing the password of another user")
        .assert { ctx =>
          withEnv { e =>
            ctx.performRequest(e.routes).status.code shouldBe Status.Forbidden.code
          }
        }
    )
  )

  path(path = "/api/users/{id}/stats")(
    supports(
      GET,
      pathParameters = p[UUID]("id", "User ID"),
      description = "Get the games statistics of a user",
      summary = "Get user stats",
      tags = Seq("users")
    )(
      onRequest(pathParameters = creator.id.unwrap)
        .respondsWith[List[Payloads.Response.UserGameStats]](Status.Ok, description = "Return the user statistics for each game type")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.Ok.code
            response.body.map(s => (s.game, s.won, s.draw, s.lost)) shouldBe List((GameTyp.War, 3, 1, 2))
          }
        },
      onRequest(pathParameters = other.id.unwrap)
        .respondsWith[List[Payloads.Response.UserGameStats]](Status.Ok, description = "Return an empty list when the user has not played yet")
        .assert { ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.Ok.code
            response.body shouldBe empty
          }
        },
      onRequest(pathParameters = unknownId.unwrap).respondsWith[ApiMessage](Status.NotFound, description = "Return 404 when the user does not exist").assert {
        ctx =>
          withEnv { e =>
            val response = ctx.performRequest(e.routes)

            response.status.code shouldBe Status.NotFound.code
            response.body.code shouldBe "not_found"
          }
      }
    )
  )
