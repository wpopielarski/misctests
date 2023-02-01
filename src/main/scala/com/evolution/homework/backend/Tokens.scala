package com.evolution.homework.backend

final case class Tokens(amount: Int) extends AnyVal {
  def +(other: Tokens): Tokens = Tokens(amount + other.amount)
}

object Tokens {
  def zero: Tokens = Tokens(0)
}
