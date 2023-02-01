package com.evolution.homework.backend

sealed abstract class Suit private (val value: Char) extends Ordered[Suit] {
  override def toString: String = value.toString
  override def compare(that: Suit): Int = 0
}

object Suit {
  case object Clubs extends Suit('c')
  case object Hearts extends Suit('h')
  case object Spades extends Suit('s')
  case object Diamonds extends Suit('d')
  def values = Seq(Clubs, Hearts, Spades, Diamonds)
}
