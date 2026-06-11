package io.tyoras.cards.domain.auth

import cats.effect.*
import pdi.jwt.JwtClaim
import cats.syntax.all.*
import io.tyoras.cards.domain.auth.model.TokenExpiration

import java.time.Clock

trait JwtExpiration[F[_]]:
  def expiresIn(claim: JwtClaim, exp: TokenExpiration): F[JwtClaim]

object JwtExpiration:
  def make[F[_] : Sync]: F[JwtExpiration[F]] =
    Sync[F].delay(Clock.systemUTC()).map { case given Clock =>
      (claim: JwtClaim, exp: TokenExpiration) => Sync[F].delay(claim.issuedNow.expiresIn(exp.toMillis))
    }
