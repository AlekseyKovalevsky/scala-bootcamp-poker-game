package akovalevsky.scalabootcamp.poker.gameengine

object Common {

  implicit class StringOps(str: String) {
    def error: Error = Error(str)
  }

  final case class Error(message: String) extends AnyVal

}
