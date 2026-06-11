package io.tyoras.cards.domain.auth.config

import io.tyoras.cards.domain.auth.model.TokenExpiration
import pdi.jwt.JwtAlgorithm
import pdi.jwt.algorithms.JwtHmacAlgorithm

final case class AuthConfig(secretKey: String, exp: TokenExpiration):
  val hmacAlgo: JwtHmacAlgorithm = JwtAlgorithm.HS256
