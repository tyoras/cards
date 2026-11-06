package io.tyoras.cards.cli.remote.client

import cats.data.NonEmptyList
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import io.tyoras.cards.cli.fixtures.{*, given}
import io.tyoras.cards.cli.remote.auth.AuthCredentials
import io.tyoras.cards.cli.remote.auth.AuthError.UserNotAuthenticated
import io.tyoras.cards.cli.remote.client.ClientError.{CallError, TransformationError}
import io.tyoras.cards.cli.remote.config.CardsClientConfig
import io.tyoras.cards.cli.remote.fakes.gameId
import io.tyoras.cards.domain.auth.model.Password
import io.tyoras.cards.domain.game.GameTyp.War
import io.tyoras.cards.domain.user.model.User
import io.tyoras.cards.shared.endpoint.auth.Payloads as AuthPayloads
import io.tyoras.cards.shared.endpoint.games.Payloads as GamesPayloads
import io.tyoras.cards.shared.endpoint.users.Payloads as UsersPayloads
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.client.{Client, UnexpectedStatus}
import org.http4s.headers.{Accept, Authorization}
import org.http4s.implicits.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class HttpClientsSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val config = CardsClientConfig(uri"http://cards.test", uri"ws://cards.test")

  /** A client recording the received requests and answering with the given function */
  private def recordingClient(respond: Request[IO] => IO[Response[IO]]): IO[(Client[IO], IO[List[Request[IO]]])] =
    Ref.of[IO, List[Request[IO]]](Nil).map { ref =>
      val app = HttpApp[IO](req => req.toStrict(None).flatMap(r => ref.update(_ :+ r)) >> respond(req))
      Client.fromHttpApp(app) -> ref.get
    }

  private def ok[A : io.circe.Encoder](body: A): IO[Response[IO]] = IO.pure(Response[IO](Status.Ok).withEntity(body.asJson))
  private def status(s: Status): IO[Response[IO]]                 = IO.pure(Response[IO](s))

  private def userPayload(u: User.Existing): UsersPayloads.Response.User =
    UsersPayloads.Response.User(u.id, u.createdAt, u.updatedAt, u.name, u.about)

  private def gamePayload(n: Int): GamesPayloads.Response.Game =
    GamesPayloads.Response.Game(gameId(n), now, now, War, NonEmptyList.of(alice.id, bob.id))

  private val password = Password.applyUnsafe("secret-password")

  "AuthClient" should "login with the user credentials" in {
    for
      (client, requests) <- recordingClient(_ => ok(AuthPayloads.Response.SuccessfulLogin(token, alice.id)))
      creds              <- AuthClient.make[IO](config, client).login(alice.name, password)
      reqs               <- requests
      body               <- reqs.head.as[AuthPayloads.Request.Login]
    yield
      creds shouldBe AuthCredentials(alice.id, token)
      reqs.map(r => r.method -> r.uri) shouldBe List(Method.POST -> uri"http://cards.test/auth/login")
      body shouldBe AuthPayloads.Request.Login(Some("Alice"), Some("secret-password"))
  }

  it should "fail when the login is refused" in {
    for
      (client, _) <- recordingClient(_ => status(Status.Unauthorized))
      result      <- AuthClient.make[IO](config, client).login(alice.name, password).attempt
    yield result.left.toOption.get shouldBe a[UnexpectedStatus]
  }

  "UsersClient" should "list all the users" in {
    for
      (client, requests) <- recordingClient(_ => ok(List(userPayload(alice), userPayload(bob))))
      users              <- UsersClient.make[IO](config, client).listAll
      reqs               <- requests
    yield
      users shouldBe List(alice, bob)
      reqs.map(r => r.method -> r.uri) shouldBe List(Method.GET -> uri"http://cards.test/api/users")
  }

  it should "find a user by id" in {
    for
      (client, requests) <- recordingClient(_ => ok(userPayload(bob)))
      user               <- UsersClient.make[IO](config, client).findById(bob.id)
      reqs               <- requests
    yield
      user shouldBe Some(bob)
      reqs.map(_.uri) shouldBe List(uri"http://cards.test/api/users" / bob.id.toString)
  }

  it should "not find an unknown user" in {
    for
      (client, _) <- recordingClient(_ => status(Status.NotFound))
      user        <- UsersClient.make[IO](config, client).findById(bob.id)
    yield user shouldBe None
  }

  it should "find users by partial name" in {
    for
      (client, requests) <- recordingClient(_ => ok(List(userPayload(carol))))
      users              <- UsersClient.make[IO](config, client).findByPartialName("Car")
      reqs               <- requests
    yield
      users shouldBe List(carol)
      reqs.map(_.uri) shouldBe List(uri"http://cards.test/api/users?name=Car")
  }

  it should "wrap the call errors" in {
    for
      (client, _) <- recordingClient(_ => status(Status.Forbidden))
      client      <- IO(UsersClient.make[IO](config, client))
      results     <- List(client.listAll.void, client.findById(alice.id).void, client.findByPartialName("a").void).traverse(_.attempt)
    yield results.foreach(r => r.left.toOption.get shouldBe a[CallError])
  }

  it should "report the users that cannot be transformed to the domain model" in {
    val invalid = userPayload(alice).copy(name = " ", about = "")
    for
      (client, _) <- recordingClient(_ => ok(List(invalid)))
      result      <- UsersClient.make[IO](config, client).listAll.attempt
    yield result.left.toOption.get match
      case e: TransformationError =>
        e.errors.toList should not be empty
        e.getMessage should startWith("Client error: Error while transforming the response to domain model:")
      case other => fail(s"unexpected error $other")
  }

  it should "report the user that cannot be transformed to the domain model" in {
    for
      (client, _) <- recordingClient(_ => ok(userPayload(alice).copy(about = " ")))
      result      <- UsersClient.make[IO](config, client).findById(alice.id).attempt
    yield result.left.toOption.get shouldBe a[TransformationError]
  }

  it should "report the users found by name that cannot be transformed to the domain model" in {
    for
      (client, _) <- recordingClient(_ => ok(List(userPayload(alice).copy(name = ""))))
      result      <- UsersClient.make[IO](config, client).findByPartialName("a").attempt
    yield result.left.toOption.get shouldBe a[TransformationError]
  }

  it should "not implement the user management operations yet" in {
    for
      (client, _) <- recordingClient(_ => status(Status.Ok))
      users       <- IO(UsersClient.make[IO](config, client))
      results     <- List(IO(users.createNewUser(alice.data)), IO(users.removeUserById(alice.id)), IO(users.upsertUser(alice))).traverse(_.attempt)
    yield results.foreach(r => r.left.toOption.get shouldBe a[NotImplementedError])
  }

  "ClientError" should "describe the call errors" in
    IO(CallError(new Exception("boom")).getMessage).asserting(_ shouldBe "Client error: Error while making the request: boom")

  "GamesClient" should "list all the games" in {
    for
      (client, requests) <- recordingClient(_ => ok(List(gamePayload(1), gamePayload(2))))
      games              <- GamesClient.make[IO](config, client, noWsClient, authProvider()).listAll
      reqs               <- requests
    yield
      games.map(_.id) shouldBe List(gameId(1), gameId(2))
      games.head shouldBe Game(gameId(1), now, now, War, NonEmptyList.of(alice.id, bob.id))
      reqs.map(r => r.method -> r.uri) shouldBe List(Method.GET -> uri"http://cards.test/api/games")
  }

  it should "find a game by id" in {
    for
      (client, requests) <- recordingClient(_ => ok(gamePayload(3)))
      game               <- GamesClient.make[IO](config, client, noWsClient, authProvider()).findById(gameId(3))
      reqs               <- requests
    yield
      game.map(_.id) shouldBe Some(gameId(3))
      reqs.map(_.uri) shouldBe List(uri"http://cards.test/api/games" / gameId(3).toString)
  }

  it should "not find an unknown game" in {
    for
      (client, _) <- recordingClient(_ => status(Status.NotFound))
      game        <- GamesClient.make[IO](config, client, noWsClient, authProvider()).findById(gameId(3))
    yield game shouldBe None
  }

  it should "find the games of a user" in {
    for
      (client, requests) <- recordingClient(_ => ok(List(gamePayload(4))))
      games              <- GamesClient
        .make[IO](config, client, noWsClient, authProvider())
        .pipe(c => c.findByUserId(alice.id, finished = false) >> c.findByUserId(alice.id, finished = true))
      reqs <- requests
    yield
      games.map(_.id) shouldBe List(gameId(4))
      reqs.map(_.uri.renderString) shouldBe List(
        s"http://cards.test/api/games?user_id=${alice.id}",
        s"http://cards.test/api/games?user_id=${alice.id}&finished"
      )
  }

  it should "remove a game" in {
    for
      (client, requests) <- recordingClient(req =>
        status(if req.uri.path.segments.last.toString == gameId(1).toString then Status.NoContent else Status.NotFound)
      )
      games      <- IO(GamesClient.make[IO](config, client, noWsClient, authProvider()))
      removed    <- games.removeById(gameId(1))
      notRemoved <- games.removeById(gameId(2))
      reqs       <- requests
    yield
      removed shouldBe true
      notRemoved shouldBe false
      reqs.map(_.method).distinct shouldBe List(Method.DELETE)
  }

  it should "create a war game" in {
    for
      (client, requests) <- recordingClient(_ => ok(gamePayload(5)))
      game               <- GamesClient.make[IO](config, client, noWsClient, authProvider()).createWarGame(NonEmptyList.of(alice.id, bob.id))
      reqs               <- requests
      body               <- reqs.head.as[Json]
    yield
      game.id shouldBe gameId(5)
      reqs.map(r => r.method -> r.uri) shouldBe List(Method.POST -> uri"http://cards.test/api/games/war")
      body shouldBe Json.obj("players" -> Json.arr(alice.id.toString.asJson, bob.id.toString.asJson))
  }

  "authedClient" should "add the authentication and accept headers" in {
    for
      (client, requests) <- recordingClient(_ => status(Status.Ok))
      ok                 <- authedClient[IO](client, authProvider()).successful(Request[IO](Method.GET, uri"http://cards.test/ping"))
      reqs               <- requests
    yield
      ok shouldBe true
      val headers = reqs.head.headers
      headers.get[Authorization] shouldBe Some(Authorization(Credentials.Token(AuthScheme.Bearer, token.value)))
      headers.get[Accept].map(_.values.head.mediaRange) shouldBe Some(MediaType.application.json)
  }

  it should "fail when the user is not authenticated" in {
    for
      (client, requests) <- recordingClient(_ => status(Status.Ok))
      result             <- authedClient[IO](client, unauthenticated).successful(Request[IO](Method.POST, uri"http://cards.test/ping")).attempt
      reqs               <- requests
    yield
      result shouldBe Left(UserNotAuthenticated)
      reqs shouldBe empty
  }

  extension [A](a: A) private def pipe[B](f: A => B): B = f(a)
