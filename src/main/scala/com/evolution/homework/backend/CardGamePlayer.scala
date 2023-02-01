package com.evolution.homework.backend

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2.text
import io.circe._
import io.circe.generic.auto._
import io.circe.literal._
import org.http4s.EntityDecoder
import org.http4s.Status
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.client.Client
import org.http4s.client.dsl.io._
import org.http4s.ember.client._
import org.http4s.implicits._

import java.util.UUID
import scala.concurrent.duration._

class CardGamePlayer(client: Client[IO], root: String, player: Player) {
  def getResponse[S <: Id[S], T <: Waiting](msg: S, endpoint: String)(implicit
      reqEnc: EntityEncoder[IO, S],
      resDec: EntityDecoder[IO, T]
  ): IO[T] = {
    val post = Request[IO](
      method = Method.POST,
      uri = Uri.unsafeFromString(s"$root/$endpoint"),
      body = reqEnc.toEntity(msg).body
    )
    client
      .expect(post)(resDec)
      .flatMap {
        case value if !value.waits => IO(value)
        case value =>
          IO.println("Waiting for other player move.") *> cats.effect
            .Temporal[IO]
            .sleep(
              5.second
            ) *> getResponse(msg.copyId(value.gameId), endpoint)
      }
  }

  def joinGame(gameType: GameType): IO[GameId] =
    getResponse(JoinGameReq(gameType.name, None, player.value), "join")(
      circeEntityEncoder[IO, JoinGameReq],
      circeEntityDecoder[IO, GameIdRes]
    ).flatMap(res => IO(GameId(UUID.fromString(res.gameId.get))))

  def makeDecision(gameId: GameId, decision: CardGameDecision): IO[ResultRes] =
    getResponse(
      DecisionReq(Some(gameId.id.toString), player.value, decision.name),
      "decide"
    )(
      circeEntityEncoder[IO, DecisionReq],
      circeEntityDecoder[IO, ResultRes]
    )

  def getPlayerCards(gameId: GameId): IO[GetCardsRes] =
    getResponse(GetCardsReq(Some(gameId.id.toString), player.value), "cards")(
      circeEntityEncoder[IO, GetCardsReq],
      circeEntityDecoder[IO, GetCardsRes]
    )

  def award(gameId: GameId): IO[AwardRes] =
    getResponse(ResultReq(Some(gameId.id.toString), player.value), "award")(
      circeEntityEncoder[IO, ResultReq],
      circeEntityDecoder[IO, AwardRes]
    )
}
