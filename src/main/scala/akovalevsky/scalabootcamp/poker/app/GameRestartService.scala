package akovalevsky.scalabootcamp.poker.app

import akovalevsky.scalabootcamp.poker.app.GameRoom.GameRoomState
import akovalevsky.scalabootcamp.poker.app.Utils.ListOps
import akovalevsky.scalabootcamp.poker.gameengine.Cards.{Card, Deck}
import akovalevsky.scalabootcamp.poker.gameengine.Common.LimitHoldemSettings
import akovalevsky.scalabootcamp.poker.gameengine.HandEvaluator._
import akovalevsky.scalabootcamp.poker.gameengine.Common.Player
import akovalevsky.scalabootcamp.poker.gameengine.{Game, HandEvaluator}
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Timer}
import cats.syntax.all._

import scala.concurrent.duration.DurationInt

object GameRestartService {
  def of[F[_]](stateRef: Ref[F, GameRoomState])
              (implicit S: Concurrent[F], T: Timer[F], settings: LimitHoldemSettings): F[Unit] = {
    val checkInterval = 5.seconds

    def writeGameLog(log: String): F[Unit] =
      stateRef.update(state => state.copy(gameLog = log :: state.gameLog))

    def writeGameCompletionLog(winners: List[(Player, HandEvaluationResult)]): F[Unit] = {
      val winnersStr = winners.map { case (player, evaluationResult) =>
        (player.id,
          evaluationResult.hand.cards.sorted.reverse.map(_.toString).mkString(""),
          evaluationResult.combination.toString
        )
      }.mkString(",")

      writeGameLog(s"The game is completed. Winners are: $winnersStr")
    }

    def defineGameLeaders: F[List[(Player, HandEvaluationResult)]] = for {
      state <- stateRef.get
      winners <- {
        def score(playerWithEvaluatedHand: (Player, HandEvaluationResult)): Int =
          playerWithEvaluatedHand match {
            case (_, evaluationResult) => evaluationResult.score
          }

        val playersSortedByScoreDesc = state.game.currentBettingRound.activePlayers
          .map(player => (player, HandEvaluator.findBestHand(state.game.pockets(player), state.game.board)))
          .sortBy(score)
          .reverse

        playersSortedByScoreDesc.takeWhile(score(_) == score(playersSortedByScoreDesc.head))
      }.pure
    } yield winners

    def run: F[Unit] = for {
      _ <- T.sleep(checkInterval)
      deck <- Deck.shuffled(RandomUtils.shuffle[F, Card])
      gameLeaders <- defineGameLeaders
      state <- stateRef.get
      _ <- {
        if (state.game.isCompleted)
          writeGameCompletionLog(gameLeaders) <* T.sleep(checkInterval)
        else S.unit
      }
      _ <- stateRef.updateMaybe { state =>
        if (state.game.isCompleted)
          Game.create(deck, state.players.rotateLeft(1)).map(x => state.copy(game = x)).toOption
        else
          None
      }
      _ <- run
    } yield ()

    run
  }
}