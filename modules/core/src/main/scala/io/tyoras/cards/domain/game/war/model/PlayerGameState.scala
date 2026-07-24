package io.tyoras.cards.domain.game.war.model

import cats.data.{NonEmptyList, NonEmptySet}
import cats.syntax.all.*
import io.chrisdavenport.fuuid.circe.given
import io.circe.derivation.*
import io.circe.{Codec, Decoder, Encoder}
import io.tyoras.cards.domain.card.{Card, Hand}
import io.tyoras.cards.domain.card.codecs.given
import io.tyoras.cards.domain.game.GameStateFilter
import io.tyoras.cards.domain.game.war.codecs.given
import io.tyoras.cards.domain.game.war.model.PlayerGameState.WarTurn.BattleRound
import io.tyoras.cards.domain.game.war.model.{Elimination, GameState, PlayerId, Turn}
import io.tyoras.cards.util.codecs.json.given

sealed trait PlayerGameState:
  val code: String
  val label: String
  val turn: Turn
  val playerId: PlayerId

object PlayerGameState:
  final case class Init(override val turn: Turn, override val playerId: PlayerId, ready: Set[PlayerId], notReady: Set[PlayerId]) extends PlayerGameState:
    override val code: String  = "init"
    override val label: String = "Initialisation"

  final case class BattleTurn(
      override val turn: Turn,
      override val playerId: PlayerId,
      hand: Hand,
      opponents: Map[PlayerId, Card.Count],
      playedCards: Map[PlayerId, Card],
      missingPlays: Set[PlayerId]
  ) extends PlayerGameState:
    override val code: String            = "battle-turn"
    override val label: String           = s"Waiting for players [${missingPlays.mkString(", ")}] to play"
    lazy val pickFirstCard: Option[Card] = hand.headOption

  final case class WarTurn(
      override val turn: Turn,
      override val playerId: PlayerId,
      hand: Hand,
      opponents: Map[PlayerId, Card.Count],
      battles: NonEmptyList[WarTurn.BattleRound],
      missingHidden: Set[PlayerId],
      missingPlays: Set[PlayerId]
  ) extends PlayerGameState:
    override val code: String            = "war-turn"
    lazy val currentRound: BattleRound   = battles.last
    override val label: String           = s"War battle round ${battles.size - 1} between players [${currentRound.involvedPlayers.mkString_(", ")}]"
    lazy val pickFirstCard: Option[Card] = hand.headOption

  object WarTurn:
    final case class BattleRound(involvedPlayers: NonEmptySet[PlayerId], hiddenPlayedCards: Map[PlayerId, Card], fightingCards: Map[PlayerId, Card])

  final case class PlayerWinTurn(
      override val turn: Turn,
      override val playerId: PlayerId,
      hand: Hand,
      opponents: Map[PlayerId, Card.Count],
      winnerId: PlayerId,
      wonCards: Set[Card],
      eliminated: Set[PlayerId],
      acked: Set[PlayerId],
      notAcked: Set[PlayerId]
  ) extends PlayerGameState:
    override val code: String  = "player-win-turn"
    override val label: String = s"Player $winnerId has won the turn and won ${wonCards.size} cards."

  final case class Finish(override val turn: Turn, override val playerId: PlayerId, winnerId: PlayerId, eliminations: List[Elimination])
      extends PlayerGameState:
    override val code: String  = "finish"
    override val label: String = "Finish"

  final case class Exit(override val turn: Turn, override val playerId: PlayerId) extends PlayerGameState:
    override val code: String  = "exit"
    override val label: String = "Exit"

  given ConfiguredCodec[BattleRound]     = ConfiguredCodec.derived
  given ConfiguredCodec[PlayerGameState] = ConfiguredCodec.derive(discriminator = Some("code"), transformMemberNames = renaming.snakeCase)
  given GameStateFilter[GameState] with
    type PlayerState = PlayerGameState
    override def codec: Codec[PlayerGameState] = summon[ConfiguredCodec[PlayerGameState]]
    extension (gameState: GameState)
      def filterForPlayer(playerId: PlayerId): PlayerGameState =
        gameState match
          case s: GameState.Init       => Init(s.context.turnNumber, playerId, s.ready, s.notReady)
          case s: GameState.BattleTurn =>
            BattleTurn(
              s.context.turnNumber,
              playerId,
              s.context.playerHand(playerId),
              s.context.playersCardCount,
              s.playedCards,
              s.missingPlays
            )
          case s: GameState.WarTurn =>
            val battles = s.battles.map(b => WarTurn.BattleRound(b.involvedPlayers, b.hiddenPlayedCards, b.fightingCards))
            WarTurn(
              s.context.turnNumber,
              playerId,
              s.context.playerHand(playerId),
              s.context.playersCardCount,
              battles,
              s.missingHidden,
              s.missingPlays
            )
          case s: GameState.PlayerWinTurn =>
            PlayerWinTurn(
              s.context.turnNumber,
              playerId,
              s.context.playerHand(playerId),
              s.context.playersCardCount,
              s.winnerId,
              s.wonCards,
              s.eliminated,
              s.acked,
              s.notAcked
            )
          case s: GameState.Finish => Finish(s.context.turnNumber, playerId, s.winnerId, s.context.eliminations)
          case s: GameState.Exit   => Exit(s.context.turnNumber, playerId)
