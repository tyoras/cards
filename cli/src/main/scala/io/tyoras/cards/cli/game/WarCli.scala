package io.tyoras.cards.cli.game

import io.tyoras.cards.game.War.{divide, play}
import io.tyoras.cards.{international52Deck, shuffle}

object WarCli {

  def game(): Unit = {
    println("War")
    play(divide(shuffle(international52Deck)))
  }

}
