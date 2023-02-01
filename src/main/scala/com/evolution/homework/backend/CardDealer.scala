package com.evolution.homework.backend

import cats.effect.IO
import scala.util.Random

trait CardDealer {
  def deal(cardsPerPlayer: Int): List[Card]
}

object RandomDealer extends CardDealer {
  val rand = new Random(1234)
  override def deal(cardsPerPlayer: Int): List[Card] =
    rand
      .shuffle(Rank.values)
      .take(cardsPerPlayer)
      .zip(rand.shuffle(Suit.values).take(cardsPerPlayer))
      .map { case (rank, suit) =>
        Card(rank, suit)
      }
      .toList

}
