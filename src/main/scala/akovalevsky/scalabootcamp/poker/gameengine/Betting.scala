package akovalevsky.scalabootcamp.poker.gameengine

import cats.data.{NonEmptySet, Validated, ValidatedNec}
import cats.syntax.all._

import java.util.UUID

object Betting {

  object TexasLimitHoldemSettings {
    // stake must be equal to the small stake or the big stake depending on the round number
    val stake = 10
    val minPlayerCount = 2
    val maxPlayerCount = 9
  }

  type Error = String

  case class BettingPlayer(id: UUID) extends AnyVal

  case class BettingRound private(
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

    private def tryCompleteRound(round: BettingRound): BettingRound =
      if (round.currentBettingPlayer == round.activePlayers.head &&
        round.betAmountsByPlayer.values.forall(_ == round.betAmountsByPlayer.values.head))
        round.copy(isCompleted = true)
      else
        round.copy()

    private def makeBlinds(): BettingRound = {
      val smallBlindPlayer = this.currentBettingPlayer
      val bigBlindPlayer = getNextBettingPlayer(this.activePlayers, this.currentBettingPlayer)

      this.copy(
        currentBettingPlayer = getNextBettingPlayer(this.activePlayers, bigBlindPlayer),
        betAmountsByPlayer = betAmountsByPlayer +
          (smallBlindPlayer -> TexasLimitHoldemSettings.stake / 2) +
          (bigBlindPlayer -> TexasLimitHoldemSettings.stake))
    }

    private def validateIsNotCompleted(round: BettingRound): Either[Error, BettingRound] =
      if (!round.isCompleted)
        Right(round.copy())
      else
        Left("Unable to act. The round is completed.")

    private def validateNoBetsMade(round: BettingRound): Either[Error, BettingRound] =
      if (round.betAmountsByPlayer.isEmpty)
        Right(round.copy())
      else
        Left("Unable to act. There are bets made.")

    private def validateNoHigherBets(round: BettingRound): Either[Error, BettingRound] = {
      val highestBet = betAmountsByPlayer.values.max
      val currentPlayerBet = betAmountsByPlayer(round.currentBettingPlayer)

      if (highestBet == currentPlayerBet)
        Right(round.copy())
      else
        Left("Unable to act. There is a higher bet made.")
    }

    def check(): Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted(this)
        round <- validateNoBetsMade(round)
        round <- switchNextBettingPlayer(round).asRight[Error]
        round <- tryCompleteRound(round).asRight[Error]
      } yield round

    private def call(round: BettingRound): BettingRound = {
      val highestBet: Int = round.betAmountsByPlayer.values.max
      this.copy(
        betAmountsByPlayer = round.betAmountsByPlayer + (round.currentBettingPlayer -> highestBet))
    }

    def call(): Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted(this)
        round <- validateNoHigherBets(round)
        round <- call(round).asRight[Error]
        round <- switchNextBettingPlayer(round).asRight[Error]
        round <- tryCompleteRound(round).asRight[Error]
      } yield round


    private def raise(round: BettingRound): BettingRound = {
      val highestBet: Int = round.betAmountsByPlayer.values.max
      this.copy(
        betAmountsByPlayer = round.betAmountsByPlayer + (round.currentBettingPlayer -> (highestBet + TexasLimitHoldemSettings.stake)))
    }

    def raise(): Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted(this)
        round <- raise(round).asRight[Error]
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
        round <- tryCompleteRound(round).asRight[Error]
      } yield round
  }

  object BettingRound {
    def validateMinPlayerCount(players: List[BettingPlayer]): ValidatedNec[Error, List[BettingPlayer]] =
      if (players.length < TexasLimitHoldemSettings.minPlayerCount)
        players.validNec
      else
        "Can't create the betting round: two or more players can play.".invalidNec

    def validateMaxPlayerCount(players: List[BettingPlayer]): ValidatedNec[Error, List[BettingPlayer]] =
      if (players.length > TexasLimitHoldemSettings.maxPlayerCount)
        players.validNec
      else
        "Can't create the betting round: two or more players can play.".invalidNec

    def validateCurrentPlayerAmongActive(player: BettingPlayer, activePlayers: List[BettingPlayer]): ValidatedNec[Error, BettingPlayer] =
      if (activePlayers.contains(player))
        player.validNec
      else
        "Can't create the betting round: the current betting player unknown.".invalidNec

    def create(activePlayers: List[BettingPlayer], currentBettingPlayer: BettingPlayer): ValidatedNec[Error, BettingRound] = {
      val activePlayersValidated = validateMinPlayerCount(activePlayers) *> validateMaxPlayerCount(activePlayers)
      val currentBettingPlayerValidated = validateCurrentPlayerAmongActive(currentBettingPlayer, activePlayers)

      (activePlayersValidated, currentBettingPlayerValidated).mapN {
        case (activePlayers, currentBettingPlayer) =>
          val bets = activePlayers.map(_ -> 0).toMap
          new BettingRound(activePlayers, currentBettingPlayer, bets, false)
      }
    }

    def createWithBlinds(activePlayers: List[BettingPlayer], currentBettingPlayer: BettingPlayer): ValidatedNec[Error, BettingRound] =
      for {
        round <- create(activePlayers, currentBettingPlayer)
      } yield round.makeBlinds()
  }

}


