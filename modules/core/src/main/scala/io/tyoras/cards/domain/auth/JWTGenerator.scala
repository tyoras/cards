package io.tyoras.cards.domain.auth

import cats.effect.Sync
import pdi.jwt.*
import dev.profunktor.auth.jwt.*
import cats.syntax.all.*
import io.chrisdavenport.fuuid.circe.*
import io.circe.syntax.*
import io.tyoras.cards.domain.user.User

trait JWTGenerator[F[_]]:
  def create(user: User.Existing): F[JwtToken]

object JWTGenerator:
  def make[F[_] : Sync](jwtExpiration: JwtExpiration[F], conf: AuthConfig): JWTGenerator[F] =
    (user: User.Existing) =>
      for
        claim <- jwtExpiration.expiresIn(JwtClaim(UserClaim(user.id).asJson.noSpaces), conf.exp)
        secretKey = JwtSecretKey(conf.secretKey.getBytes)
        token <- jwtEncode(claim, secretKey, conf.hmacAlgo)
      yield token
