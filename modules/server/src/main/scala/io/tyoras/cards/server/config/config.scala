package io.tyoras.cards.server.config

import cats.effect.Sync
import org.http4s.headers.Origin
import pureconfig.error.CannotConvert
import pureconfig.module.catseffect.syntax.*
import pureconfig.{ConfigReader, ConfigSource}

def parseConfig[F[_] : Sync](configSource: ConfigSource): F[ServerConfig] =
  configSource.loadF[F, ServerConfig]()

given ConfigReader[Set[Origin.Host]] =
  ConfigReader.fromString[Set[Origin.Host]] {
    case "*" => Right(Set.empty) // empty set means all origins are allowed
    case s =>
      Origin
        .parse(s)
        .fold(
          err => Left(CannotConvert(s, "org.http4s.headers.Origin.Host", err.sanitized)),
          {
            case Origin.HostList(hosts) => Right(hosts.toList.toSet)
            case Origin.Null            => Left(CannotConvert(s, "org.http4s.headers.Origin.Host", "Expected a set of origin host but found 'null'"))
          }
        )
  }
