package com.evolution.homework.backend

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import com.evolution.homework.backend.CardGameDecision.{Fold, Play}
import com.evolution.homework.backend.GameType.{DoubleCardGame, SingleCardGame}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers._

import java.util.UUID
import cats.effect.kernel.Ref
import scala.collection.mutable.{Map => MMap}

class CardGameCroupierTest extends AsyncFreeSpec with AsyncIOSpec {
  private def scenario(
      gameType: GameType,
      cA: List[Card],
      cB: List[Card],
      decisionA: CardGameDecision,
      decisionB: CardGameDecision,
      tokensA: Int,
      tokensB: Int
  ): IO[Unit] = {
    val A = Player("A")
    val B = Player("B")

    val gameId = GameId(UUID.randomUUID())
    val idGenerator = new CardGameIdGenerator {
      override def generate: GameId = gameId
    }
    val playersRef = Ref.of[IO, MMap[Player, Tokens]](MMap.empty)
    val gamesRef = Ref.of[IO, MMap[GameId, GameDescriptor]](MMap.empty)

    val dealer = new CardDealer {
      import scala.collection.mutable.Stack
      val behavior = Stack(cA, cB)
      override def deal(cardsPerPlayer: Int): List[Card] = behavior.pop
    }

    for {
      players <- playersRef
      games <- gamesRef

      val croupier = CardGameCroupier.create(players, games, dealer)

      _ <- CardGameServer.getOrCreatePlayer(players, A.value)
      _ <- CardGameServer.getOrCreatePlayer(players, B.value)

      playersMap <- players.get
      gamesMap <- games.get

      // initially games map is empty
      _ = gamesMap shouldBe empty
      _ = playersMap(A) shouldBe Tokens(0)
      _ = playersMap(B) shouldBe Tokens(0)

      // join to game
      _ <- croupier.joinGame(idGenerator, JoinGameReq(gameType.name, None, A.value))
      _ = gamesMap(gameId) shouldBe GameDescriptor(Some(gameType), Some(gameId), Steps.JOIN, Set(A), Map.empty, Map.empty)
      _ <- croupier.joinGame(idGenerator, JoinGameReq(gameType.name, None, B.value))
      _ = gamesMap(gameId) shouldBe GameDescriptor(Some(gameType), Some(gameId), Steps.JOIN, Set(A, B), Map.empty, Map.empty)
      
      // get cards
      _ <- croupier.getPlayerCards(GetCardsReq(Some(gameId.id.toString), A.value))
      _ = gamesMap(gameId) shouldBe GameDescriptor(Some(gameType), Some(gameId), Steps.CARDS, Set(A, B), Map.empty, Map(A -> cA))
      _ <- croupier.getPlayerCards(GetCardsReq(Some(gameId.id.toString), B.value))
      _ = gamesMap(gameId) shouldBe GameDescriptor(Some(gameType), Some(gameId), Steps.CARDS, Set(A, B), Map.empty, Map(A -> cA, B -> cB))

      // make decision
      _ <- croupier.makeDecision(DecisionReq(Some(gameId.id.toString), A.value, decisionA.name))
      _ <- croupier.makeDecision(DecisionReq(Some(gameId.id.toString), B.value, decisionB.name))
      // and allow to see A the B's decision too
      _ <- croupier.makeDecision(DecisionReq(Some(gameId.id.toString), A.value, decisionA.name))

      // both decisions are made, the game can end and winnings calculated and applied to balances,
      // see awards if both player decided to continue to play,
      // if any of player folds 'award' is not invoked
      _ <- croupier.award(ResultReq(Some(gameId.id.toString), A.value)).attempt
      _ <- croupier.award(ResultReq(Some(gameId.id.toString), B.value)).attempt

      // check that tokens were rightly calculated as game is finished
      _ = playersMap(A) shouldBe Tokens(tokensA)
      _ = playersMap(B) shouldBe Tokens(tokensB)
    } yield ()
  }

  private val As = Card(Rank.Ace, Suit.Spades)
  private val Ac = Card(Rank.Ace, Suit.Clubs)
  private val Js = Card(Rank.Jack, Suit.Spades)
  private val Jc = Card(Rank.Jack, Suit.Clubs)
  private val Ah = Card(Rank.Ace, Suit.Hearts)
  private val Th = Card(Rank.Ten, Suit.Hearts)

  "SingleCardGame" - {
    "both fold" in scenario(SingleCardGame, List(As), List(Th), Fold, Fold, -1, -1)
    "one plays" in scenario(SingleCardGame, List(As), List(Th), Fold, Play, -3, +3)
    "both play" in scenario(SingleCardGame, List(As), List(Th), Play, Play, +10, -10)
    "draw" in scenario(SingleCardGame, List(As), List(Ac), Play, Play, 0, 0)
  }

  "DoubleCardGame" - {
    "both fold" in scenario(DoubleCardGame, List(As, Js), List(Ah, Th), Fold, Fold, -2, -2)
    "one plays" in scenario(DoubleCardGame, List(As, Js), List(Ah, Th), Fold, Play, -5, +5)
    "both play" in scenario(DoubleCardGame, List(As, Js), List(Ah, Th), Play, Play, +20, -20)
    "draw" in scenario(DoubleCardGame, List(As, Js), List(Ac, Jc), Play, Play, 0, 0)
  }
}
