package io.tyoras.cards

import cats.effect.Sync
//import pureconfig.generic.ProductHint
//import pureconfig.generic.auto.*
//import pureconfig.module.catseffect.syntax.CatsEffectConfigSource
//import pureconfig.{CamelCase, ConfigFieldMapping, ConfigSource, SnakeCase}

import java.nio.file.Path

package object config:
//  implicit def hint[T]: ProductHint[T] = ProductHint[T](ConfigFieldMapping(CamelCase, SnakeCase))

  def parseConfig[F[_] : Sync](configPath: Path): F[CardsConfig] =
//    ConfigSource.default(ConfigSource.file(configPath)).loadF[F, CardsConfig]()
    Sync[F].pure(
      CardsConfig(
        ServerConfig(
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
