package com.evolution.homework.backend

sealed trait CardGameDecision {
  def name: String
}

object CardGameDecision {
  def fromName(name: String): CardGameDecision = name match {
    case Play.name => Play
    case Fold.name => Fold
    case _ => throw new IllegalArgumentException(s"unknown decision: $name")
  }

  final case object Play extends CardGameDecision {
    val name = "play"
  }
  final case object Fold extends CardGameDecision {
    val name = "fold"
  }
}
