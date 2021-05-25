package akovalevsky.scalabootcamp.poker.app

object Utils {

  implicit class ListOps[A](list: List[A]) {
    def rotateLeft(n: Int): List[A] = rotateLeft(n, list)

    private def rotateLeft(n: Int, list: List[A]): List[A] = {
      val length = list.length
      list.drop(n % length) ++ list.take(n % length)
    }
  }

}
