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

object PlayerGame extends IOApp {
  import CardGameServer._

  def getPlayerTokens(
      client: Client[IO],
      player: Player
  ): IO[Tokens] =
    client
      .get(s"$ROOT/tokens/${player.value}") { res =>
        res.as[Tokens]
      }

  override def run(args: List[String]): IO[ExitCode] = {
    args match {
      case playerName :: _ =>
        IO.println(s"Welcome $playerName in casino") *> makeClient
          .use { client =>
            val player =
              new CardGamePlayer(
                client,
                s"$ROOT/game",
                Player(playerName)
              )
            getPlayerTokens(client, Player(playerName))
              .map { tokens =>
                s"You have ${tokens.amount} so far."
              }
              .flatMap(IO.println) *> IO.println(
              "Select game: SingleCard or DoubleCard"
            ) *> IO.consoleForIO.readLine
              .flatMap {
                case GameType.SingleCardGame.name => IO(GameType.SingleCardGame)
                case GameType.DoubleCardGame.name => IO(GameType.DoubleCardGame)
                case other =>
                  IO.raiseError(
                    new IllegalArgumentException(
                      s"$other is not the game I know."
                    )
                  )
              }
              .flatMap { gameType =>
                player.joinGame(gameType)
              }
              .flatMap { gameId =>
                IO.println(s"Starting game with id ${gameId.id}") *>
                  IO.println("Dealing cards...") *>
                  player.getPlayerCards(gameId).flatMap { cards =>
                    IO.println(
                      s"You got ${cards.cards.map(_.toString).mkString(", ")} cards."
                    )
                  } *>
                  IO.println(
                    "Write 'play' if you play or 'fold' if you pass (skip quotas)"
                  ) *> IO.consoleForIO.readLine
                    .flatMap {
                      case CardGameDecision.Play.name => IO(CardGameDecision.Play)
                      case CardGameDecision.Fold.name => IO(CardGameDecision.Fold)
                      case other =>
                        IO.raiseError(
                          new IllegalArgumentException(
                            s"$other is not correct decision"
                          )
                        )
                    }
                    .flatMap { decision =>
                      player.makeDecision(gameId, decision)
                    }
                    .flatMap {
                      case ResultRes(id, Some(earned), _) =>
                        IO.println(
                          s"This time you earned $earned credits. See you next time."
                        )
                      case ResultRes(id, None, _) =>
                        IO.println("Cards checking...") *>
                          player.award(gameId).flatMap { award =>
                            IO.println(
                              s"This time you earned ${award.earned.getOrElse(0)} credits. Your opponent cards: ${award.opponentCards
                                  .mkString(", ")}. See you next time."
                            )
                          }
                      case other =>
                        IO.raiseError(
                          new IllegalArgumentException(s"Unexpected state $other")
                        )
                    }
              } *> getPlayerTokens(client, Player(playerName))
              .map { tokens =>
                s"Your total tokens number is ${tokens.amount}."
              }
              .flatMap(IO.println)
          } *> IO(
          ExitCode.Success
        )
      case _ =>
        IO.println("Player's name is needed") *> IO(ExitCode.Error)
    }
  }

  def makeClient = EmberClientBuilder
    .default[IO]
    .build
}
