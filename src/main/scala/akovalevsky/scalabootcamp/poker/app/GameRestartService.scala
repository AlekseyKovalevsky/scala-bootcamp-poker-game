package akovalevsky.scalabootcamp.poker.app

import akovalevsky.scalabootcamp.poker.app.GameRoom.{GameRoomState, createNewGame}
import akovalevsky.scalabootcamp.poker.gameengine.Cards.Deck
import akovalevsky.scalabootcamp.poker.gameengine.Common.{Error, LimitHoldemSettings, Player}
import akovalevsky.scalabootcamp.poker.gameengine.Game
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Timer}
import cats.syntax.all._

import scala.concurrent.duration.DurationInt

object GameRestartService {

  private def createNewGame(players: List[Player])(implicit settings: LimitHoldemSettings): Either[Error, Game] =
    for {
      deck <- Deck.shuffle.asRight[Error] // wrap as effect
      game <- Game.create(deck, players)
    } yield game

  def of[F[_]](stateRef: Ref[F, GameRoomState])
              (implicit S: Concurrent[F], T: Timer[F], settings: LimitHoldemSettings): F[Unit] = {
    val checkInterval = 5.seconds

    def run: F[Unit] = for {
      _ <- T.sleep(checkInterval)
      _ <- stateRef.updateMaybe { state =>
        if (state.game.isCompleted)
          createNewGame(state.players)
            .map(game => GameRoomState(state.players, game))
            .toOption
        else
          None
      }
      _ <- run
    } yield ()

    run
  }
}