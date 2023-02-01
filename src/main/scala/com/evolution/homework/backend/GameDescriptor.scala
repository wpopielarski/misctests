package com.evolution.homework.backend

final case class GameDescriptor(
    gameType: Option[GameType],
    gameId: Option[GameId],
    step: Steps.Step,
    players: Set[Player],
    decisions: Map[Player, CardGameDecision],
    cards: Map[Player, List[Card]]
)

object Steps extends Enumeration {
  type Step = Value
  val JOIN, DECISION, CARDS, AWARD = Value
}
