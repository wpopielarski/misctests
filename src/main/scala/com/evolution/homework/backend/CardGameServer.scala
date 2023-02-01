package com.evolution.homework.backend

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect._
import com.comcast.ip4s._
import io.circe.generic.auto._
import io.circe.syntax._
import fs2._
import org.http4s.HttpRoutes
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.dsl.request
import org.http4s.ember.server._
import org.http4s.implicits._

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration._
import scala.util.Random

object CardGameServer extends IOApp {
  val ROOT = "http://localhost:8080"
  val IPv4 = ipv4"0.0.0.0"
  val PORT = port"8080"

  override def run(args: List[String]): IO[ExitCode] = runGames

  def runGames: IO[ExitCode] =
    Ref.of[IO, MMap[Player, Tokens]](MMap.empty).flatMap { players =>
      Ref.of[IO, MMap[GameId, GameDescriptor]](MMap.empty).flatMap { games =>
        EmberServerBuilder
          .default[IO]
          .withHost(IPv4)
          .withPort(PORT)
          .withHttpApp(gamesService(players, games))
          .build
          .use(_ => IO.never)
          .as(ExitCode.Success)
      }
    }

  def gamesService(
      players: Ref[IO, MMap[Player, Tokens]],
      games: Ref[IO, MMap[GameId, GameDescriptor]]
  ) = HttpRoutes
    .of[IO] {
      case GET -> Root / "tokens" / player =>
        getOrCreatePlayer(players, player).flatMap(tokens => Ok(tokens.asJson))
      case req @ POST -> Root / "game" / step =>
        val courpier = new CardGameCroupier(players, games, RandomDealer)
        step match {
          case "join" =>
            req.as[JoinGameReq].flatMap { joinReq =>
              courpier
                .joinGame(UUIDGameIdGenerator, joinReq)
                .flatMap(res => Ok(res.asJson))
            }
          case "decide" =>
            req.as[DecisionReq].flatMap { decisionReq =>
              courpier.makeDecision(decisionReq).flatMap(res => Ok(res.asJson))
            }
          case "cards" =>
            req.as[GetCardsReq].flatMap { cardsReq =>
              courpier.getPlayerCards(cardsReq).flatMap(res => Ok(res.asJson))
            }
          case "award" =>
            req.as[ResultReq].flatMap { resReq =>
              courpier.award(resReq).flatMap(res => Ok(res.asJson))
            }
          case other =>
            BadRequest(s"$other is not allowed step in game")
        }
    }
    .orNotFound

  def getOrCreatePlayer(
      players: Ref[IO, MMap[Player, Tokens]],
      player: String
  ): IO[Tokens] =
    players.get.flatMap { playersMap =>
      val playerOb = Player(player)
      val tokens = playersMap.get(playerOb).getOrElse(Tokens(0))
      playersMap.put(playerOb, tokens)
      IO(tokens)
    }
}
