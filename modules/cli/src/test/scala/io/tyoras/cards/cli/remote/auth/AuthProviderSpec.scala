package io.tyoras.cards.cli.remote.auth

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Ref}
import io.tyoras.cards.cli.fixtures.{*, given}
import io.tyoras.cards.cli.remote.auth.AuthError.{LoginFailure, UserNotAuthenticated}
import io.tyoras.cards.cli.remote.client.AuthClient
import io.tyoras.cards.cli.remote.config.AuthConfig
import io.tyoras.cards.domain.auth.model.Password
import io.tyoras.cards.domain.user.model.User
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class AuthProviderSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val config = AuthConfig(alice.name, Password.applyUnsafe("secret-password"))

  private def authClient(result: IO[AuthCredentials], calls: Ref[IO, List[(User.Name, Password)]]): AuthClient[IO] =
    (userName, password) => calls.update(_ :+ (userName -> password)) >> result

  "AuthProvider" should "login once at creation and provide the credentials" in {
    for
      calls    <- Ref.of[IO, List[(User.Name, Password)]](Nil)
      provider <- AuthProvider.make[IO](config, authClient(IO.pure(AuthCredentials(alice.id, token)), calls))
      creds1   <- provider.connectedUserCredentials
      creds2   <- provider.connectedUserCredentials
      logins   <- calls.get
    yield
      creds1 shouldBe AuthCredentials(alice.id, token)
      creds2 shouldBe creds1
      logins shouldBe List(config.userName -> config.password)
  }

  it should "fail to provide the credentials when the login failed" in {
    for
      calls    <- Ref.of[IO, List[(User.Name, Password)]](Nil)
      provider <- AuthProvider.make[IO](config, authClient(IO.raiseError(LoginFailure("bad password")), calls))
      result   <- provider.connectedUserCredentials.attempt
    yield result shouldBe Left(UserNotAuthenticated)
  }

  "AuthError" should "describe the error" in {
    IO((UserNotAuthenticated.getMessage, LoginFailure("detail").getMessage)).asserting { (notAuthenticated, failure) =>
      notAuthenticated shouldBe "Auth error: User is not authenticated"
      failure shouldBe "Auth error: "
    }
  }
