package io.tyoras.cards.server.config

import cats.effect.Sync

import pureconfig.module.catseffect.syntax.*
import pureconfig.ConfigSource

def parseConfig[F[_] : Sync](configSource: ConfigSource): F[ServerConfig] =
  configSource.loadF[F, ServerConfig]()
