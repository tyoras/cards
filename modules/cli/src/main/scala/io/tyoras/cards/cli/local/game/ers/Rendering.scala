package io.tyoras.cards.cli.local.game.ers

import cats.effect.Sync
import cats.effect.std.Console
import ErsCliError.InvalidState
import io.tyoras.cards.cli.lineSeparator
import io.tyoras.cards.domain.game.ers.model.GameState
import io.tyoras.cards.domain.game.ers.model.GameState.*
import org.typelevel.log4cats.LoggerFactory
import cats.syntax.all.*
import io.tyoras.cards.domain.game.ers.model.PlayerId

trait Rendering[F[_]]:
  def renderGameState(state: GameState, playerId: PlayerId): F[Unit]

object Rendering:
  def apply[F[_] : Sync : LoggerFactory](playerNames: Map[PlayerId, String])(using console: Console[F]): Rendering[F] =
    val logger = LoggerFactory.getLogger

    def playerName(playerId: PlayerId): String =
      playerNames.getOrElse(playerId, s"Unknown player ($playerId)")

    def renderPlayerTurn(s: PlayerTurn, playerId: PlayerId): F[Unit] =
      val ctx               = s.context
      val currentPlayerName = playerName(playerId)
      val nextCard          = ctx.pickFirstCard(playerId).getOrElse("unknow card")
      console.println(s"Turn ${ctx.turnNumber}: Battle") >>
        console.println(s"Played cards: ${s.pile.reverse.mkString_(" ")}") >>
        console.println(s"\n> $currentPlayerName this is your turn to play a card.\n") >>
        console.println(s"Press 'Enter' to play your next card $nextCard ...") >>
        console.println(s"Press 's' to slap the pile!").whenA(s.pile.nonEmpty)

    def renderChallengeTurn(s: ChallengeTurn, playerId: PlayerId): F[Unit] =
      val ctx               = s.context
      val currentPlayerName = playerName(playerId)
      val action            = "play a card"
      val nextCard          = ctx.pickFirstCard(playerId).getOrElse("unknown card")
      console.println(s"Turn ${ctx.turnNumber}: ${playerName(s.owner)} has challenged ${playerName(s.challenger)}") >>
        console.println(s"${playerName(s.challenger)} has ${s.remainingTries} remaining tries") >>
        console.println(s"Played cards: ${s.pile.reverse.mkString_(" ")}") >>
        console.println(s"\n> $currentPlayerName this is your turn to $action.\n") >>
        console.println(s"Press 'Enter' to play your next card $nextCard ...") >>
        console.println(s"Press 's' to slap the pile!")

    def renderSlapPenalty(s: SlapPenalty, playerId: PlayerId): F[Unit] =
      val ctx               = s.context
      val currentPlayerName = playerName(playerId)
      val player            = if s.player == playerId then "You have" else s"Player ${playerName(s.player)} has"
      val nextCard          = ctx.pickFirstCard(s.player).getOrElse("unknow card")
      console.println(s"Turn ${ctx.turnNumber}: $player been penalized for slapping too early!") >>
        console.println(s"Discarded cards: ${s.discardedCards.reverse.mkString_(" ")}") >>
        console.println(s"\n> $currentPlayerName you have been penalized for slapping too early.\n") >>
        (if s.player == playerId then console.println(s"Press 'Enter' to discard $nextCard") else console.println(s"Press 'Enter' to continue..."))

    def renderPlayerWinTurn(s: PlayerWinTurn, playerId: PlayerId): F[Unit] =
      val ctx    = s.context
      val winner = if s.winnerId == playerId then "You have" else s"Player ${playerName(s.winnerId)} has"
      console.println(s"$winner won turn ${ctx.turnNumber} and won ${s.wonCards.size} cards.") >>
        console
          .println(s"Player(s) eliminated this turn are: ${s.eliminated.toList.map(playerName).mkString_("\n\t- ", "\n\t- ", "\n")}")
          .whenA(s.eliminated.nonEmpty) >>
        console.println("Press 'Enter' when ready to start the new turn")

    def renderFinish(s: Finish): F[Unit] =
      val ctx = s.context
      console.println(s"Game finished after ${ctx.turnNumber} turns.\n") >>
        console.println(s"The winner is ${playerName(s.winnerId)} !\n") >>
        ctx.eliminations.zipWithIndex.traverse_((elimination, rank) =>
          console.println(s"${rank + 2}. ${playerName(elimination.playerId)} eliminated at turn ${elimination.turn}")
        ) >>
        console.println(s"\nUse \\q to quit the game or \\r to restart a new game with same players...")

    (state: GameState, playerId: PlayerId) =>
      console.println(lineSeparator) >>
        (state match
          case _: Init          => console.println("Press 'Enter' when you are ready to start the game...")
          case s: PlayerTurn    => renderPlayerTurn(s, playerId)
          case s: ChallengeTurn => renderChallengeTurn(s, playerId)
          case s: SlapPenalty   => renderSlapPenalty(s, playerId)
          case s: PlayerWinTurn => renderPlayerWinTurn(s, playerId)
          case s: Finish        => renderFinish(s)
          // Exit state is handled exclusively by the main game loop
          case _: Exit => logger.error("Rendering Exit state is not supported") *> InvalidState.raiseError)
