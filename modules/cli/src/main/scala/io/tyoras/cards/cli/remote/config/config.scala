package io.tyoras.cards.cli.remote.config

import cats.effect.kernel.Sync
import pureconfig.module.catseffect.syntax.*
import pureconfig.ConfigSource

def parseConfig[F[_] : Sync]: F[RemoteCliConfig] =
  ConfigSource.resources("cards-cli.conf").loadF[F, RemoteCliConfig]()
