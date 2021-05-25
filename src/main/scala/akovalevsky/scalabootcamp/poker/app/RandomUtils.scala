package akovalevsky.scalabootcamp.poker.app

import cats.effect.Sync

import scala.util.Random

object RandomUtils {
  private val random = new Random()

  def shuffle[F[_] : Sync, A](list: List[A]): F[List[A]] = Sync[F].delay(random.shuffle(list))

  def checkChance[F[_] : Sync](chance: Double): F[Boolean] =
    Sync[F].delay(chance > random.between(0d, 1d))
}
