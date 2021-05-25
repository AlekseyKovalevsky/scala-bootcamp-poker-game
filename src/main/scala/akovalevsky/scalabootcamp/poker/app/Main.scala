package akovalevsky.scalabootcamp.poker.app

import akovalevsky.scalabootcamp.poker.app.GameRoom.GameRoomState
import akovalevsky.scalabootcamp.poker.gameengine.Betting.BettingRoundName
import akovalevsky.scalabootcamp.poker.gameengine.Betting.BettingRoundName._
import akovalevsky.scalabootcamp.poker.gameengine.Common.LimitHoldemSettings
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.concurrent.duration.DurationInt

object Main extends IOApp {
  implicit val settings: LimitHoldemSettings = new LimitHoldemSettings {
    override val lowerLimit: Int = 10
    override val higherLimit: Int = 20
    override val minPlayerCount: Int = 2
    override val maxPlayerCount: Int = 9
  }

  override def run(args: List[String]): IO[ExitCode] = {

    def visibleCardsCount(round: BettingRoundName): Int = round match {
      case Preflop => 0
      case Flop => 3
      case Turn => 4
      case River => 5
    }

    def printGameRoomState(state: GameRoomState): IO[Unit] =
      IO {
        print("\u001b[2J")
        println(s"LOG: ${state.gameLog.headOption.getOrElse("None")}")
        println(s"ROUND: ${state.game.currentBettingRound.name} ")
        println(s"BETTING PLAYER: ${state.game.currentBettingRound.currentBettingPlayer.id} ")
        println(s"POT: ${state.game.pot}$$")
        println(s"Board:")
        val board = state.game.board.cards
          .take(visibleCardsCount(state.game.currentBettingRound.name))
          .sorted.reverse
          .map(_.toString)
          .mkString("")
        println(board)
        println(s"Players:")
        val players = state.game.currentBettingRound.activePlayers.map { player =>
          (player.id,
            state.game.pockets(player).cards.sorted.reverse.map(_.toString).mkString(""),
            s"${state.game.currentBettingRound.betAmountsByPlayer(player)}$$")
        }.mkString(" ")

        println(players)
      }

    (for {
      gameRoomOrErr <- GameRoom.of[IO]
      _ <- gameRoomOrErr.map { gameRoom =>
        (for {
          _ <- IO.sleep(2.seconds)
          state <- gameRoom.spectate
          _ <- printGameRoomState(state)
        } yield ()).foreverM
      }.sequence
    } yield ()) as ExitCode.Success

  }


}
