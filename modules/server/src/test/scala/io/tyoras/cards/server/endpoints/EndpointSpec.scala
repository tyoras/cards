package io.tyoras.cards.server.endpoints

import org.http4s.{HttpRoutes, Request}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import pl.iterators.baklava.http4s.BaklavaHttp4s
import pl.iterators.baklava.scalatest.{BaklavaScalatest, ScalatestAsExecution}
import cats.effect.IO
import io.tyoras.cards.domain.game.GameType

import java.time.ZonedDateTime
import cats.effect.unsafe.IORuntime
import dev.profunktor.auth.jwt.JwtToken
import pl.iterators.baklava.{HttpBearer, PrimitiveSchema, Schema, SchemaType, SecurityScheme}
import pl.iterators.kebs.baklava.schema.KebsBaklavaSchema

trait EndpointSpec
    extends AnyFunSpec with Matchers with BaklavaHttp4s[Unit, Unit, ScalatestAsExecution]
    with BaklavaScalatest[HttpRoutes[IO], BaklavaHttp4s.ToEntityMarshaller, BaklavaHttp4s.FromEntityUnmarshaller] with KebsBaklavaSchema:
  def testedRoutes: HttpRoutes[IO]

  given Schema[JwtToken] = PrimitiveSchema[JwtToken]("JwtToken", SchemaType.StringType, None)

  given Schema[ZonedDateTime] = PrimitiveSchema[ZonedDateTime]("ZonedDateTime", SchemaType.StringType, Some("date-time"))

  given Schema[GameType] = PrimitiveSchema[GameType]("GameType", SchemaType.StringType, None)

  implicit override val runtime: IORuntime = IORuntime.global

  override def strictHeaderCheckDefault = false

  override def performRequest(routes: HttpRoutes[IO], request: Request[IO]): HttpResponse =
    routes.orNotFound.run(request).unsafeRunSync()

  val bearer: HttpBearer           = HttpBearer(bearerFormat = "JWT")
  val bearerScheme: SecurityScheme = SecurityScheme("bearer", bearer)
