package akovalevsky.scalabootcamp.poker.gameengine

import cats.data.ValidatedNec
import cats.syntax.all._
import akovalevsky.scalabootcamp.poker.gameengine.Common._

import java.util.UUID

object Betting {

  trait LimitHoldemRoundSettings {
    val stake: Int
    val minPlayerCount: Int
    val maxPlayerCount: Int
  }


  /*implicit val defaultSettings: LimitHoldemRoundSettings = new LimitHoldemRoundSettings {
    override val stake: Int = 10
    override val minPlayerCount: Int = 2
    override val maxPlayerCount: Int = 9
  }*/

  case class BettingPlayer(id: UUID) extends AnyVal

  case class BettingRound private(
                                   activePlayers: List[BettingPlayer],
                                   currentBettingPlayer: BettingPlayer,
                                   betAmountsByPlayer: Map[BettingPlayer, Int],
                                   isCompleted: Boolean
                                 )(implicit settings: LimitHoldemRoundSettings) {
    private def nextBettingPlayer: BettingPlayer = {
      val nextBettingPlayerIdx = (activePlayers.indexOf(currentBettingPlayer) + 1) % activePlayers.length

      activePlayers(nextBettingPlayerIdx)
    }

    private def switchNextBettingPlayer: BettingRound =
      copy(currentBettingPlayer = nextBettingPlayer)

    private def tryComplete: BettingRound =
      if (currentBettingPlayer == activePlayers.head &&
        betAmountsByPlayer.values.forall(_ == betAmountsByPlayer.values.head))
        copy(isCompleted = true)
      else
        this

    private def makeBlinds: BettingRound = {
      val smallBlindPlayer = currentBettingPlayer
      val bigBlindPlayer = nextBettingPlayer

      switchNextBettingPlayer.switchNextBettingPlayer
        .copy(betAmountsByPlayer = betAmountsByPlayer +
          (smallBlindPlayer -> settings.stake / 2) +
          (bigBlindPlayer -> settings.stake))
    }

    private def validateIsNotCompleted: Either[Error, BettingRound] =
      if (!isCompleted)
        Right(this)
      else
        Left("Unable to act. The round is completed.".error)

    private def validateNoBetsMade: Either[Error, BettingRound] =
      if (betAmountsByPlayer.isEmpty)
        Right(this)
      else
        Left("Unable to act. There are bets made.".error)

    private def validateNoHigherBets: Either[Error, BettingRound] = {
      val highestBet = betAmountsByPlayer.values.max
      val currentPlayerBet = betAmountsByPlayer(currentBettingPlayer)

      if (highestBet == currentPlayerBet)
        Right(this)
      else
        Left("Unable to act. There is a higher bet made.".error)
    }

    def check: Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted
        round <- round.validateNoBetsMade
      } yield round.switchNextBettingPlayer.tryComplete

    private def callHighestBet: BettingRound = {
      val highestBet: Int = betAmountsByPlayer.values.max
      copy(
        betAmountsByPlayer = betAmountsByPlayer + (currentBettingPlayer -> highestBet))
    }

    def call: Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted
        round <- round.validateNoHigherBets
      } yield round.callHighestBet.switchNextBettingPlayer.tryComplete


    private def raiseToHighestBet: BettingRound = {
      val highestBet: Int = betAmountsByPlayer.values.max
      copy(
        betAmountsByPlayer = betAmountsByPlayer + (currentBettingPlayer -> (highestBet + settings.stake)))
    }

    def raise: Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted
      } yield round.raiseToHighestBet.switchNextBettingPlayer

    private def withoutCurrentBettingPlayer: BettingRound =
      copy(
        activePlayers = activePlayers.filterNot(_ == currentBettingPlayer),
        currentBettingPlayer = nextBettingPlayer)

    def fold: Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted
      } yield round.withoutCurrentBettingPlayer.tryComplete
  }

  object BettingRound {
    private def validateMinPlayerCount(players: List[BettingPlayer])
                                      (implicit gameSettings: LimitHoldemRoundSettings): ValidatedNec[Error, List[BettingPlayer]] =
      if (players.length < gameSettings.minPlayerCount)
        players.validNec
      else
        "Can't create the betting round: two or more players can play.".error.invalidNec

    private def validateMaxPlayerCount(players: List[BettingPlayer])
                                      (implicit gameSettings: LimitHoldemRoundSettings): ValidatedNec[Error, List[BettingPlayer]] =
      if (players.length > gameSettings.maxPlayerCount)
        players.validNec
      else
        "Can't create the betting round: two or more players can play.".error.invalidNec

    private def validateCurrentPlayerAmongActive(player: BettingPlayer, activePlayers: List[BettingPlayer]): ValidatedNec[Error, BettingPlayer] =
      if (activePlayers.contains(player))
        player.validNec
      else
        "Can't create the betting round: the current betting player unknown.".error.invalidNec

    def create(activePlayers: List[BettingPlayer], currentBettingPlayer: BettingPlayer)
              (implicit gameSettings: LimitHoldemRoundSettings):
    ValidatedNec[Error, BettingRound] = {
      val activePlayersValidated = validateMinPlayerCount(activePlayers) *> validateMaxPlayerCount(activePlayers)
      val currentBettingPlayerValidated = validateCurrentPlayerAmongActive(currentBettingPlayer, activePlayers)

      (activePlayersValidated, currentBettingPlayerValidated).mapN {
        case (activePlayers, currentBettingPlayer) =>
          val bets = activePlayers.map(_ -> 0).toMap
          new BettingRound(activePlayers, currentBettingPlayer, bets, false)(gameSettings)
      }
    }

    def createWithBlinds(activePlayers: List[BettingPlayer], currentBettingPlayer: BettingPlayer)
                        (implicit gameSettings: LimitHoldemRoundSettings): ValidatedNec[Error, BettingRound] =
      for {
        round <- create(activePlayers, currentBettingPlayer)
      } yield round.makeBlinds
  }

}

