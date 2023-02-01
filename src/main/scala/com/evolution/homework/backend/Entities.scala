package com.evolution.homework.backend

trait Waiting extends Id[Waiting] {
  def waits: Boolean
}

trait Id[T <: Id[T]] {
  def gameId: Option[String]
  def copyId(gameId: Option[String]): T
}

case class JoinGameReq(gameType: String, gameId: Option[String], player: String) extends Id[JoinGameReq] {

  override def copyId(gameId: Option[String]): JoinGameReq =
    copy(gameId = gameId)

}
case class DecisionReq(gameId: Option[String], player: String, decision: String) extends Id[DecisionReq] {

  override def copyId(gameId: Option[String]): DecisionReq =
    copy(gameId = gameId)

}
case class ResultRes(
    gameId: Option[String],
    earned: Option[Int],
    waits: Boolean = false
) extends Waiting {

  override def copyId(gameId: Option[String]): Waiting = copy(gameId = gameId)

}
case class GetCardsReq(gameId: Option[String], player: String) extends Id[GetCardsReq] {

  override def copyId(gameId: Option[String]): GetCardsReq =
    copy(gameId = gameId)

}
case class GetCardsRes(
    gameId: Option[String],
    cards: List[Card],
    waits: Boolean = false
) extends Waiting {

  override def copyId(gameId: Option[String]): Waiting = copy(gameId = gameId)

}
case class ResultReq(gameId: Option[String], player: String) extends Id[ResultReq] {

  override def copyId(gameId: Option[String]): ResultReq = copy(gameId = gameId)

}
case class GameIdRes(gameId: Option[String], waits: Boolean) extends Waiting {

  override def copyId(gameId: Option[String]): Waiting = copy(gameId = gameId)

}

case class AwardRes(
    gameId: Option[String],
    earned: Option[Int],
    opponentCards: List[Card],
    waits: Boolean = false
) extends Waiting {

  override def copyId(gameId: Option[String]): Waiting = copy(gameId = gameId)

}
