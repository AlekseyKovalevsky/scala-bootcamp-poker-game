package akovalevsky.scalabootcamp.poker.app

import akovalevsky.scalabootcamp.poker.app.GameRoom.GameRoomState
import akovalevsky.scalabootcamp.poker.gameengine.Cards.{Card, Deck}
import akovalevsky.scalabootcamp.poker.gameengine.Common._
import akovalevsky.scalabootcamp.poker.gameengine.Game
import cats.Monad
import cats.effect.{Concurrent, Timer}
import cats.effect.concurrent.Ref
import cats.syntax.all._
import cats.effect.syntax.all._

class GameRoom[F[_] : Monad](stateRef: Ref[F, GameRoomState])
                            (implicit S: Concurrent[F], T: Timer[F], settings: LimitHoldemSettings) {

  private def validateNotRoomMember(player: Player, players: List[Player]): Either[Error, Unit] =
    Either.cond(!players.contains(player), (), "The player has already joined the room".error)

  private def validateRoomMember(player: Player, players: List[Player]): Either[Error, Unit] =
    Either.cond(players.contains(player), (), "The player is not found in the room".error)

  private def validateIsPlayersTurn(player: Player, game: Game): Either[Error, Unit] =
    Either.cond(
      game.currentBettingRound.currentBettingPlayer.id == player.id,
      (),
      "The player is not found in the room".error)

  private def writeGameLog(log: String): F[Unit] =
    stateRef.update(state => state.copy(gameLog = log :: state.gameLog))


  def join(player: Player): F[Either[Error, Unit]] =
    for {
      err <- stateRef.updateOr { state =>
        for {
          _ <- validateNotRoomMember(player, state.players)
        } yield state.copy(players = state.players :+ player)
      }
    } yield err.toLeft(())

  def check(player: Player): F[Either[Error, Unit]] =
    for {
      err <- stateRef.updateOr { state =>
        for {
          _ <- validateRoomMember(player, state.players)
          _ <- validateIsPlayersTurn(player, state.game)
          game <- state.game.check
        } yield state.copy(game = game)
      }
      _ <- writeGameLog(s"Player ${player.id} checks")
    } yield err.toLeft()

  def call(player: Player): F[Either[Error, Unit]] =
    for {
      err <- stateRef.updateOr { state =>
        for {
          _ <- validateRoomMember(player, state.players)
          _ <- validateIsPlayersTurn(player, state.game)
          game <- state.game.call
        } yield state.copy(game = game)
      }
      _ <- writeGameLog(s"Player ${player.id} calls")
    } yield err.toLeft()

  def raise(player: Player): F[Either[Error, Unit]] =
    for {
      err <- stateRef.updateOr { state =>
        for {
          _ <- validateRoomMember(player, state.players)
          _ <- validateIsPlayersTurn(player, state.game)
          game <- state.game.raise
        } yield state.copy(game = game)
      }
      _ <- writeGameLog(s"Player ${player.id} raises")
    } yield err.toLeft()


  def fold(player: Player): F[Either[Error, Unit]] =
    for {
      err <- stateRef.updateOr { state =>
        for {
          _ <- validateRoomMember(player, state.players)
          _ <- validateIsPlayersTurn(player, state.game)
          game <- state.game.fold
        } yield state.copy(game = game)
      }
      _ <- writeGameLog(s"Player ${player.id} folds")
    } yield err.toLeft()

  def spectate: F[GameRoomState] = stateRef.get

}

object GameRoom {

  private val botIdPrefix = "bot_"
  private val botPlayers = List(
    Player(botIdPrefix + "Vasya"), Player(botIdPrefix + "Pavel"), Player(botIdPrefix + "Alex"))

  final case class GameRoomState(players: List[Player], game: Game, gameLog: List[String])

  def of[F[_]](implicit S: Concurrent[F], T: Timer[F], settings: LimitHoldemSettings): F[Either[Error, GameRoom[F]]] = {

    for {
      deck <- Deck.shuffled(RandomUtils.shuffle[F, Card])
      gameRoom <- Game.create(deck, botPlayers).map { game =>
        for {
          stateRef <- Ref.of(GameRoomState(botPlayers, game, List()))
          gameRoom <- new GameRoom(stateRef).pure
          _ <- BotService.of[F](gameRoom, botPlayers).start // make the gameroom a resource to cancel the fiber
          _ <- GameRestartService.of[F](stateRef).start // make the gameroom a resource to cancel the fiber
        } yield gameRoom
      }.sequence
    } yield gameRoom

  }
}

