package akovalevsky.scalabootcamp.poker.gameengine

import cats.syntax.all._

import java.util.UUID

object Betting {

  type Error = String

  case class BettingPlayer(id: UUID) extends AnyVal

  case class BettingRound(
                           activePlayers: List[BettingPlayer],
                           currentBettingPlayer: BettingPlayer,
                           betAmountsByPlayer: Map[BettingPlayer, Int],
                           isCompleted: Boolean
                         ) {
    private def getNextBettingPlayer(players: List[BettingPlayer], prevPlayer: BettingPlayer): BettingPlayer = {
      val nextBettingPlayerIdx = (players.indexOf(prevPlayer) + 1) % players.length

      players(nextBettingPlayerIdx)
    }

    private def switchNextBettingPlayer(round: BettingRound): BettingRound =
      round.copy(currentBettingPlayer = getNextBettingPlayer(round.activePlayers, round.currentBettingPlayer))

    private def tryEnd(round: BettingRound): BettingRound =
      if (round.currentBettingPlayer == round.activePlayers.head &&
        round.betAmountsByPlayer.values.forall(_ == 0))
        round.copy(isCompleted = true)
      else
        round.copy()

    private def validateIsNotCompleted(round: BettingRound): Either[Error, BettingRound] =
      if (!round.isCompleted)
        Right(round.copy())
      else
        Left("Unable to act. The round is completed.")

    private def validateAnyBetsMade(round: BettingRound): Either[Error, BettingRound] =
      if (round.betAmountsByPlayer.nonEmpty)
        Right(round.copy())
      else
        Left("Unable to act. There are no bets made.")

    private def validateNoBetsMade(round: BettingRound): Either[Error, BettingRound] =
      if (round.betAmountsByPlayer.isEmpty)
        Right(round.copy())
      else
        Left("Unable to act. There are bets made.")

    def check(): Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted(this)
        round <- validateNoBetsMade(round)
        round <- switchNextBettingPlayer(round).asRight[Error]
        round <- tryEnd(round).asRight[Error]
      } yield round

    private def call(round: BettingRound): BettingRound = {
      val highestBet: Int = round.betAmountsByPlayer.values.max
      this.copy(
        betAmountsByPlayer = round.betAmountsByPlayer + (round.currentBettingPlayer -> highestBet))
    }

    def call(): Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted(this)
        round <- validateAnyBetsMade(round)
        round <- call(round).asRight[Error]
        round <- switchNextBettingPlayer(round).asRight[Error]
        round <- tryEnd(round).asRight[Error]
      } yield round

    def bet(round: BettingRound, amount: Int): BettingRound =
      round.copy(betAmountsByPlayer = betAmountsByPlayer + (round.currentBettingPlayer -> amount))

    def bet(amount: Int): Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted(this)
        round <- validateNoBetsMade(round)
        round <- bet(round, amount).asRight[Error]
      } yield round

    private def raise(round: BettingRound, byAmount: Int): BettingRound = {
      val highestBet: Int = round.betAmountsByPlayer.values.max
      this.copy(
        betAmountsByPlayer = round.betAmountsByPlayer + (round.currentBettingPlayer -> (highestBet + byAmount)))
    }

    def raise(byAmount: Int): Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted(this)
        round <- validateAnyBetsMade(round)
        round <- raise(round, byAmount).asRight[Error]
        round <- switchNextBettingPlayer(round).asRight[Error]
      } yield round

    private def removeCurrentBettingPlayer(round: BettingRound): BettingRound = {
      round.copy(
        activePlayers = round.activePlayers.filterNot(_ == round.currentBettingPlayer),
        currentBettingPlayer = getNextBettingPlayer(round.activePlayers, round.currentBettingPlayer))
    }

    def fold(): Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted(this)
        round <- removeCurrentBettingPlayer(round).asRight[Error]
        round <- tryEnd(round).asRight[Error]
      } yield round
  }

}


