package akovalevsky.scalabootcamp.poker.gameengine

object Common {

  implicit class StringOps(str: String) {
    def error: Error = Error(str)
  }

  final case class Error(message: String) extends AnyVal

  trait LimitHoldemSettings {
    val lowerLimit: Int
    val higherLimit: Int
    val minPlayerCount: Int
    val maxPlayerCount: Int
  }

  final case class Player(id: String) extends AnyVal

}
