package akovalevsky.scalabootcamp.poker.app

import akovalevsky.scalabootcamp.poker.app.GameRoom.GameRoomState
import akovalevsky.scalabootcamp.poker.gameengine.Common.Player
import cats.data.EitherT
import cats.effect.{Concurrent, Timer}
import cats.syntax.all._

import scala.concurrent.duration.DurationInt

object BotService {
  def of[F[_]](gameRoom: GameRoom[F], botPlayers: List[Player])
              (implicit S: Concurrent[F], T: Timer[F]): F[Unit] = {
    val actInterval = 5.seconds

    def act(gameRoomState: GameRoomState): F[Unit] = {
      val currentBettingPlayer = gameRoomState.game.currentBettingRound.currentBettingPlayer

      if (botPlayers.contains(currentBettingPlayer)) {
        (EitherT(gameRoom.check(currentBettingPlayer)) orElse
          EitherT(gameRoom.call(currentBettingPlayer)) orElse
          EitherT(gameRoom.fold(currentBettingPlayer))).value.void
      }
      else
        S.unit
    }

    def run: F[Unit] =
      for {
        _ <- T.sleep(actInterval)
        state <- gameRoom.spectate
        _ <- act(state)
        _ <- run
      } yield ()

    run
  }
}
