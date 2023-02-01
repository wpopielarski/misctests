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

object CardGameCroupier {
  def create(
      players: Ref[IO, MMap[Player, Tokens]],
      games: Ref[IO, MMap[GameId, GameDescriptor]],
      dealer: CardDealer
  ): CardGameCroupier = new CardGameCroupier(players, games, dealer)
}

class CardGameCroupier(
    players: Ref[IO, MMap[Player, Tokens]],
    games: Ref[IO, MMap[GameId, GameDescriptor]],
    dealer: CardDealer
) {
  def joinGame(gen: CardGameIdGenerator, req: JoinGameReq): IO[GameIdRes] =
    games.get
      .flatMap { gamesMap =>
        val (gameId, gameDesc) = req.gameId match {
          case None =>
            gamesMap
              .filter { case (gameId, desc) =>
                desc.step == Steps.JOIN && desc.gameType == GameType.fromName(req.gameType)
              }
              .toSeq
              .headOption
              .getOrElse {
                val gid = gen.generate
                (gid, GameDescriptor(None, Some(gid), Steps.JOIN, Set.empty, Map.empty, Map.empty))
              }
          case Some(gameId) =>
            val gid = GameId(UUID.fromString(gameId))
            (gid, gamesMap(gid))
        }
        val gameType = GameType.fromName(req.gameType)
        val updatedState = gameDesc.copy(
          gameType = gameType,
          players = gameDesc.players + Player(req.player)
        )
        gamesMap.put(gameId, updatedState)
        IO(updatedState)
      }
      .flatMap { desc =>
        if (
          desc.players.size == desc.gameType
            .map(_.playerPerGame)
            .getOrElse(0)
        )
          IO(GameIdRes(Some(desc.gameId.map(_.id.toString).get), false))
        else IO(GameIdRes(Some(desc.gameId.map(_.id.toString).get), true))
      }

  def makeDecision(req: DecisionReq): IO[ResultRes] =
    players.get.flatMap { playersMap =>
      games.get.flatMap { gamesMap =>
        val decision = CardGameDecision.fromName(req.decision)
        val updatedState =
          gamesMap(GameId(req.gameId.map(UUID.fromString).get)) match {
            case desc
                if desc.step == Steps.DECISION & desc.decisions.size < desc.gameType
                  .map(_.playerPerGame)
                  .get =>
              desc.copy(decisions = desc.decisions + (Player(req.player) -> decision))
            case desc if desc.step == Steps.CARDS =>
              desc.copy(step = Steps.DECISION, decisions = Map(Player(req.player) -> decision))
            case desc => desc
          }
        if (updatedState.step == Steps.DECISION)
          gamesMap.put(updatedState.gameId.get, updatedState)
        if (
          updatedState.decisions.size < updatedState.gameType
            .map(_.playerPerGame)
            .getOrElse(0)
        )
          IO(ResultRes(updatedState.gameId.map(_.id.toString), None, true))
        else {
          if (updatedState.decisions.values.toSet == Set(CardGameDecision.Play))
            IO(ResultRes(updatedState.gameId.map(_.id.toString), None))
          else if (updatedState.decisions.values.toSet == Set(CardGameDecision.Fold)) {
            playersMap.put(
              Player(req.player),
              playersMap(Player(req.player)) + Tokens(
                updatedState.gameType.map(_.foldFold).get
              )
            )
            IO(
              ResultRes(
                updatedState.gameId.map(_.id.toString),
                updatedState.gameType.map(_.foldFold)
              )
            )
          } else {
            val decision = updatedState.decisions(Player(req.player))
            val amount =
              if (decision == CardGameDecision.Play)
                updatedState.gameType.map(_.playFold).get
              else updatedState.gameType.map(_.foldPlay).get
            playersMap.put(
              Player(req.player),
              playersMap(Player(req.player)) + Tokens(amount)
            )
            IO(ResultRes(updatedState.gameId.map(_.id.toString), Some(amount)))
          }
        }
      }
    }

  def getPlayerCards(req: GetCardsReq): IO[GetCardsRes] =
    games.get.flatMap { gamesMap =>
      val updatedState =
        gamesMap(GameId(req.gameId.map(UUID.fromString).get)) match {
          case desc
              if desc.step == Steps.CARDS & desc.cards.size < desc.gameType
                .map(_.playerPerGame)
                .get =>
            val cards = dealer.deal(desc.gameType.map(_.cardsPerPlayer).get)
            desc.copy(cards = desc.cards + (Player(req.player) -> cards))
          case desc if desc.step == Steps.JOIN =>
            val cards = dealer.deal(desc.gameType.map(_.cardsPerPlayer).get)
            desc.copy(step = Steps.CARDS, cards = Map(Player(req.player) -> cards))
          case desc => desc
        }
      if (updatedState.step == Steps.CARDS)
        gamesMap.put(updatedState.gameId.get, updatedState)
      if (
        updatedState.cards.size == updatedState.gameType
          .map(_.playerPerGame)
          .getOrElse(0)
      )
        IO(GetCardsRes(req.gameId, updatedState.cards(Player(req.player)), false))
      else
        IO(GetCardsRes(req.gameId, updatedState.cards(Player(req.player)), true))
    }

  def award(req: ResultReq): IO[AwardRes] =
    games.get.flatMap { gamesMap =>
      val game = gamesMap(GameId(req.gameId.map(UUID.fromString).get))
      if (!game.decisions.values.forall(_ == CardGameDecision.Play))
        IO.raiseError(new IllegalStateException("all decisions should be 'play'"))
      else {
        val cards = game.cards
        val playerCards = cards(Player(req.player))
        val opponentCards = cards
          .filter { case (player, cards) =>
            player != Player(req.player)
          }
          .values
          .head
        val playerResult = leftWin(playerCards.zip(opponentCards))
        val gain = playerResult match {
          case 1  => game.gameType.map(typ => Tokens(typ.win)).get
          case -1 => game.gameType.map(typ => Tokens(typ.lose)).get
          case 0  => Tokens(0)
        }
        players.get.flatMap { playersMap =>
          playersMap.put(
            Player(req.player),
            playersMap(Player(req.player)) + gain
          )
          IO.println(s">> ${req.player} ${playerCards}") *> IO.println(
            s">> opponent ${opponentCards}"
          ) *>
            IO(AwardRes(req.gameId, Option(gain.amount), opponentCards))
        }
      }
    }

  @tailrec
  final def leftWin(cards: Seq[(Card, Card)]): Int = cards match {
    case Nil => 0
    case (left, right) :: tail =>
      left.compare(right) match {
        case 0  => leftWin(tail)
        case 1  => 1
        case -1 => -1
      }
  }
}
