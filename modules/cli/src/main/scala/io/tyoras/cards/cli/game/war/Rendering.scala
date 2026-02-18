package io.tyoras.cards.cli.game.war

import cats.effect.Sync
import cats.effect.std.Console
import io.tyoras.cards.cli.game.war.WarCliError.InvalidState
import io.tyoras.cards.cli.lineSeparator
import io.tyoras.cards.domain.game.war.model.GameState
import io.tyoras.cards.domain.game.war.model.GameState.*
import org.typelevel.log4cats.LoggerFactory
import cats.syntax.all.*
import io.tyoras.cards.domain.game.schnapsen.PlayerId

trait Rendering[F[_]]:
  def renderGameState(state: GameState, playerId: PlayerId): F[Unit]

object Rendering:
  def apply[F[_] : Sync : LoggerFactory](using console: Console[F]): Rendering[F] =
    val logger = LoggerFactory.getLogger

    def renderBattleTurn(s: BattleTurn, playerId: PlayerId): F[Unit] =
      val ctx               = s.context
      val currentPlayerName = ctx.playerName(playerId)
      val nextCard          = ctx.pickFirstCard(playerId).getOrElse("unknow card")
      console.println(s"Turn ${ctx.turnNumber}: Battle") >>
        s.playedCards.toList.traverse_((id, card) => console.println(s"\t- ${ctx.playerName(id)} has played card : $card")) >>
        console.println(s"\n> $currentPlayerName this is your turn to play a card.\n") >>
        console.println(s"Press 'Enter' to play your next card $nextCard ...")

    def renderWarTurn(s: WarTurn, playerId: PlayerId): F[Unit] =
      val ctx               = s.context
      val currentPlayerName = ctx.playerName(playerId)
      val round             = s.currentRound
      val missingHidden     = s.missingHidden.contains(playerId)
      val action            = if missingHidden then "give a hidden card as prize for the war" else "play a card"
      val nextCard          = if missingHidden then "[hidden]" else ctx.pickFirstCard(playerId).getOrElse("unknown card")
      console.println(s"Turn ${ctx.turnNumber}: War between ${round.involvedPlayers.map(ctx.playerName).mkString_("[", " | ", "]")}") >>
        console.println("Hidden cards:") >>
        round.hiddenPlayedCards.keys.toList.traverse_(id => console.println(s"\t- ${ctx.playerName(id)} has already played an hidden card")) >>
        console.println("Fighting cards:") >>
        round.fightingCards.toList.traverse_((id, card) => console.println(s"\t- ${ctx.playerName(id)} has played card : $card")) >>
        console.println(s"\n> $currentPlayerName this is your turn to $action.\n") >>
        console.println(s"Press 'Enter' to play your next card $nextCard ...")

    def renderPlayerWinTurn(s: PlayerWinTurn, playerId: PlayerId): F[Unit] =
      val ctx    = s.context
      val winner = if s.winnerId == playerId then "You have" else s"Player ${ctx.playerName(s.winnerId)} has"
      console.println(s"$winner won turn ${ctx.turnNumber - 1} and won ${s.wonCards.size} cards.") >>
        console
          .println(s"Player(s) eliminated this turn are: ${s.eliminated.toList.map(ctx.playerName).mkString_("\n\t- ", "\n\t- ", "\n")}")
          .whenA(s.eliminated.nonEmpty) >>
        console.println("Press 'Enter' when ready to start the new turn")

    def renderFinish(s: Finish): F[Unit] =
      val ctx = s.context
      console.println(s"Game finished after ${ctx.turnNumber - 1} turns.\n") >>
        console.println(s"The winner is ${ctx.playerName(s.winnerId)} !\n") >>
        ctx.eliminations.zipWithIndex.traverse_((elimination, rank) =>
          console.println(s"${rank + 2}. ${ctx.playerName(elimination.playerId)} eliminated at turn ${elimination.turn}")
        ) >>
        console.println(s"\nUse \\q to quit the game or \\r to restart a new game with same players...")

    (state: GameState, playerId: PlayerId) =>
      console.println(lineSeparator) >>
        (state match
          case _: Init          => console.println("Press 'Enter' when you are ready to start the game...")
          case s: BattleTurn    => renderBattleTurn(s, playerId)
          case s: WarTurn       => renderWarTurn(s, playerId)
          case s: PlayerWinTurn => renderPlayerWinTurn(s, playerId)
          case s: Finish        => renderFinish(s)
          // Exit state is handled exclusively by the main game loop
          case _: Exit => logger.error("Rendering Exit state is not supported") *> InvalidState.raiseError)
