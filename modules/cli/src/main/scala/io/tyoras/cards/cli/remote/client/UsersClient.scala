package io.tyoras.cards.cli.remote.client

import cats.effect.Async
import io.chrisdavenport.fuuid.FUUID
import io.tyoras.cards.cli.remote.config.CardsClientConfig
import io.tyoras.cards.domain.auth.UserName
import io.tyoras.cards.domain.user.User
import io.tyoras.cards.shared.endpoint.users.Payloads
import io.tyoras.cards.shared.endpoint.users.Payloads.Response.User.given
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.Request
import org.http4s.client.Client
import io.scalaland.chimney.dsl.transformInto
import cats.syntax.all.*
import org.http4s.Method.GET

trait UsersClient[F[_]]:
  def listAll: F[List[User.Existing]]
  def findById(userId: FUUID): F[Option[User.Existing]]
  def findByPartialName(name: UserName): F[List[User.Existing]]
  // TODO implement missing users client operations
  def createNewUser(user: User.Data): F[User.Existing]  = ???
  def removeUserById(userId: FUUID): F[Unit]            = ???
  def upsertUser(user: User.Existing): F[User.Existing] = ???

object UsersClient:
  def make[F[_] : Async](config: CardsClientConfig, httpClient: Client[F]): UsersClient[F] =
    new:
      private val usersApiUri                      = config.apiUri / "api" / "users"
      override def listAll: F[List[User.Existing]] =
        for
          request <- Request[F](GET, usersApiUri).pure
          result  <- httpClient.expect[List[Payloads.Response.User]](request)
        yield result.map(_.transformInto[User.Existing])

      override def findById(userId: FUUID): F[Option[User.Existing]] =
        for
          request <- Request[F](GET, usersApiUri / userId.toString).pure
          result  <- httpClient.expectOption[Payloads.Response.User](request)
        yield result.map(_.transformInto[User.Existing])

      override def findByPartialName(name: UserName): F[List[User.Existing]] =
        for
          request <- Request[F](GET, usersApiUri.withQueryParam("name", name)).pure
          result  <- httpClient.expect[List[Payloads.Response.User]](request)
        yield result.map(_.transformInto[User.Existing])
