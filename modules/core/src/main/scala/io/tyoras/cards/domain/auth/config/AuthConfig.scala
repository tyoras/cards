package io.tyoras.cards.domain.auth.config

import io.tyoras.cards.domain.auth.model.TokenExpiration
import pdi.jwt.JwtAlgorithm
import pdi.jwt.algorithms.JwtHmacAlgorithm
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.string.*

final case class AuthConfig(secretKey: String :| (Trimmed & Not[Blank]), exp: TokenExpiration):
  val hmacAlgo: JwtHmacAlgorithm = JwtAlgorithm.HS256
