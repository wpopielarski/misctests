package com.evolution.homework.backend

import cats.effect.IO
import java.util.UUID

object UUIDGameIdGenerator extends CardGameIdGenerator {
  override def generate: GameId = GameId(UUID.randomUUID)
}
