package akovalevsky.scalabootcamp.poker.app

import akovalevsky.scalabootcamp.poker.gameengine.Common.LimitHoldemSettings
import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.duration.DurationInt

object Main extends IOApp {
  implicit val settings: LimitHoldemSettings = new LimitHoldemSettings {
    override val lowerLimit: Int = 10
    override val higherLimit: Int = 20
    override val minPlayerCount: Int = 2
    override val maxPlayerCount: Int = 9
  }

  override def run(args: List[String]): IO[ExitCode] =
    GameRoom.of[IO] *> IO.sleep(10000.seconds) as ExitCode.Success
}
