package com.evolution.homework.backend

final case class Card(rank: Rank, suit: Suit) extends Ordered[Card] {
  override def toString: String = s"$rank$suit"
  override def compare(that: Card): Int =
    (rank, suit).compare((that.rank, that.suit))
}
