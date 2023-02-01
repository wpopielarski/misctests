package com.evolution.homework.backend

sealed trait GameType {
  def playerPerGame: Int
  def cardsPerPlayer: Int
  def name: String
  def foldFold: Int
  def foldPlay: Int
  def playFold: Int
  def win: Int
  def lose: Int
}

object GameType {
  def fromName(name: String): Option[GameType] = name match {
    case SingleCardGame.name => Some(SingleCardGame)
    case DoubleCardGame.name => Some(DoubleCardGame)
    case _                   => None
  }

  case object SingleCardGame extends GameType {
    override def playerPerGame: Int = 2
    override def cardsPerPlayer: Int = 1
    override val name = "SingleCard"
    override def foldFold: Int = -1
    override def foldPlay: Int = -3
    override def playFold: Int = 3
    override def lose: Int = -10
    override def win: Int = 10
  }

  case object DoubleCardGame extends GameType {
    override def playerPerGame: Int = 2
    override def cardsPerPlayer: Int = 2
    override val name = "DoubleCard"
    override def foldFold: Int = -2
    override def foldPlay: Int = -5
    override def lose: Int = -20
    override def playFold: Int = 5
    override def win: Int = 20
  }
}
