package io.tyoras.cards.server.config

import cats.effect.Sync
import io.tyoras.cards.persistence.DatabaseConfig
//import pureconfig.generic.ProductHint
//import pureconfig.generic.auto.*
//import pureconfig.module.catseffect.syntax.CatsEffectConfigSource
//import pureconfig.{CamelCase, ConfigFieldMapping, ConfigSource, SnakeCase}

import java.nio.file.Path

//  implicit def hint[T]: ProductHint[T] = ProductHint[T](ConfigFieldMapping(CamelCase, SnakeCase))

def parseConfig[F[_] : Sync](configPath: Path): F[ServerConfig] =
//    ConfigSource.default(ConfigSource.file(configPath)).loadF[F, CardsConfig]()
  Sync[F].pure(
    ServerConfig(
      HttpConfig(
        host = "0.0.0.0",
        port = 8080
      ),
      DatabaseConfig(
        host = "localhost",
        port = 5432,
        user = "cards",
        password = "password",
        db = "cards",
        maxSession = 10
      )
    )
  )
