package com.evolution.homework.backend

sealed abstract class Rank private (val value: Char) extends Ordered[Rank] {
  def intValue: Int
  override def toString: String = value.toString
  override def compare(that: Rank): Int = intValue.compare(that.intValue)
}

object Rank {
  case object Two extends Rank('2') {
    override def intValue: Int = 2
  }

  case object Three extends Rank('3') {
    override def intValue: Int = 3
  }

  case object Four extends Rank('4') {
    override def intValue: Int = 4
  }

  case object Five extends Rank('5') {
    override def intValue: Int = 5
  }

  case object Six extends Rank('6') {
    override def intValue: Int = 6
  }

  case object Seven extends Rank('7') {
    override def intValue: Int = 7
  }

  case object Eight extends Rank('8') {
    override def intValue: Int = 8
  }

  case object Nine extends Rank('9') {
    override def intValue: Int = 9
  }

  case object Ten extends Rank('T') {
    override def intValue: Int = 10
  }

  case object Jack extends Rank('J') {
    override def intValue: Int = 11
  }

  case object Queen extends Rank('Q') {
    override def intValue: Int = 12
  }

  case object King extends Rank('K') {
    override def intValue: Int = 13
  }

  case object Ace extends Rank('A') {
    override def intValue: Int = 14
  }

  def values = Seq(
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace
  )
}
