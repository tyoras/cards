package io.tyoras.cards.shared.endpoint.admin.health

import io.circe.derivation.ConfiguredCodec
import io.circe.{Decoder, Encoder}
import io.scalaland.chimney.Transformer
import io.tyoras.cards.domain.healthcheck.HealthStatus as DomainHealthStatus
import io.tyoras.cards.util.codecs.json.given

object Payloads:
  object Response:
    final case class HealthStatus(name: String, status: String, components: List[HealthStatus]) derives ConfiguredCodec
    object HealthStatus:
      given Transformer[DomainHealthStatus, Response.HealthStatus] =
        Transformer.define[DomainHealthStatus, Response.HealthStatus].enableMethodAccessors.withFieldComputed(_.status, _.status.toString).buildTransformer
