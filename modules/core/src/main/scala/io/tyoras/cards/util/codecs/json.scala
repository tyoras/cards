package io.tyoras.cards.util.codecs

import io.circe.derivation.Configuration

object json:
  given Configuration = Configuration.default.withSnakeCaseMemberNames
