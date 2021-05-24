package akovalevsky.scalabootcamp.poker.gameengine

import akovalevsky.scalabootcamp.poker.gameengine.Betting.BettingRoundName._
import akovalevsky.scalabootcamp.poker.gameengine.Betting._
import akovalevsky.scalabootcamp.poker.gameengine.Cards._
import akovalevsky.scalabootcamp.poker.gameengine.Common._
import cats.syntax.all._

case class Game private(
                         board: Board,
                         players: List[Player],
                         pockets: Map[Player, Pocket],
                         currentBettingRound: BettingRound,
                         pot: Int,
                         isCompleted: Boolean)
                       (implicit settings: LimitHoldemSettings) {

  private def nextBettingRoundName: Option[BettingRoundName] =
    currentBettingRound.name match {
      case Preflop => Flop.some
      case Flop => Turn.some
      case Turn => River.some
      case River => None
    }

  private def validateIsNotCompleted: Either[Error, Game] =
    if (!isCompleted)
      Right(this)
    else
      Left("Unable to act. The game is completed.".error)

  private def trySwitchNextRoundOrComplete: Either[Error, Game] = {
    if (currentBettingRound.isCompleted) {
      if (nextBettingRoundName.isEmpty || currentBettingRound.activePlayers.length == 1)
        copy(
          isCompleted = true,
          pot = this.pot + currentBettingRound.betAmountsByPlayer.values.sum).asRight
      else {
        for {
          nextBettingRound <- BettingRound.create(nextBettingRoundName.get, this.currentBettingRound.activePlayers)
        } yield copy(
          currentBettingRound = nextBettingRound,
          pot = this.pot + currentBettingRound.betAmountsByPlayer.values.sum)
      }
    }
    else
      this.asRight
  }

  def check: Either[Error, Game] =
    for {
      game <- validateIsNotCompleted
      round <- game.currentBettingRound.check
      game <- game.copy(currentBettingRound = round).trySwitchNextRoundOrComplete
    } yield game

  def call: Either[Error, Game] =
    for {
      game <- validateIsNotCompleted
      round <- game.currentBettingRound.call
      game <- game.copy(currentBettingRound = round).trySwitchNextRoundOrComplete
    } yield game

  def raise: Either[Error, Game] =
    for {
      game <- validateIsNotCompleted
      round <- game.currentBettingRound.raise
    } yield game.copy(currentBettingRound = round)

  def fold: Either[Error, Game] = {
    for {
      game <- validateIsNotCompleted
      round <- game.currentBettingRound.fold
      game <- game.copy(currentBettingRound = round).trySwitchNextRoundOrComplete
    } yield game
  }
}

object Game {
  def create(deck: Deck, players: List[Player])(implicit limitHoldemSettings: LimitHoldemSettings): Either[Error, Game] = {
    for {
      board <- Board.create(deck.cards.take(5))
      pockets <- deck.cards.drop(5).grouped(2).take(players.length).map(Pocket.create).toList.sequence
      initialBettingRound <- BettingRound.createInitial(players)
    } yield new Game(
      board, players, players.zip(pockets).toMap, initialBettingRound, 0, false)
  }
}