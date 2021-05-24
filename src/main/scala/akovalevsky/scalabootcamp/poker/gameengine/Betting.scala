package akovalevsky.scalabootcamp.poker.gameengine

import cats.data.ValidatedNec
import cats.syntax.all._
import akovalevsky.scalabootcamp.poker.gameengine.Common._

object Betting {

  trait BettingRoundName

  object BettingRoundName {

    case object Preflop extends BettingRoundName

    case object Flop extends BettingRoundName

    case object Turn extends BettingRoundName

    case object River extends BettingRoundName

  }

  final case class BettingRound private(
                                         name: BettingRoundName,
                                         activePlayers: List[Player],
                                         currentBettingPlayer: Player,
                                         betAmountsByPlayer: Map[Player, Int],
                                         isCompleted: Boolean
                                       )(implicit settings: LimitHoldemSettings) {
    private def activePlayerBets: List[Int] = activePlayers.map(betAmountsByPlayer(_))

    private def currentStake: Int = name match {
      case BettingRoundName.Preflop => settings.lowerLimit
      case BettingRoundName.Flop => settings.lowerLimit
      case BettingRoundName.Turn => settings.higherLimit
      case BettingRoundName.River => settings.higherLimit
    }

    private def nextBettingPlayer: Player = {
      val nextBettingPlayerIdx = (activePlayers.indexOf(currentBettingPlayer) + 1) % activePlayers.length

      activePlayers(nextBettingPlayerIdx)
    }

    private def switchNextBettingPlayer: BettingRound =
      copy(currentBettingPlayer = nextBettingPlayer)

    private def tryComplete: BettingRound =
      if (currentBettingPlayer == activePlayers.head &&
        activePlayerBets.forall(_ == activePlayerBets.head))
        copy(isCompleted = true)
      else
        this

    private def makeBlinds: BettingRound = {
      val smallBlindPlayer = currentBettingPlayer
      val bigBlindPlayer = nextBettingPlayer

      switchNextBettingPlayer.switchNextBettingPlayer
        .copy(betAmountsByPlayer = betAmountsByPlayer +
          (smallBlindPlayer -> currentStake / 2) +
          (bigBlindPlayer -> currentStake))
    }

    private def validateIsNotCompleted: Either[Error, BettingRound] =
      if (!isCompleted)
        Right(this)
      else
        Left("Unable to act. The round is completed.".error)

    private def validateNoHigherBet: Either[Error, BettingRound] = {
      val highestBet = betAmountsByPlayer.values.max
      val currentPlayerBet = betAmountsByPlayer(currentBettingPlayer)

      if (highestBet == currentPlayerBet)
        Right(this)
      else
        Left("Unable to act. There is a higher bet made.".error)
    }

    private def validateHigherBetExist: Either[Error, BettingRound] = {
      val highestBet = betAmountsByPlayer.values.max
      val currentPlayerBet = betAmountsByPlayer(currentBettingPlayer)

      if (highestBet > currentPlayerBet)
        Right(this)
      else
        Left("Unable to act. There is no higher bet made.".error)
    }

    def check: Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted
        round <- round.validateNoHigherBet
      } yield round.switchNextBettingPlayer.tryComplete

    private def doCall: BettingRound = {
      val highestBet: Int = betAmountsByPlayer.values.max
      copy(
        betAmountsByPlayer = betAmountsByPlayer + (currentBettingPlayer -> highestBet))
    }

    def call: Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted
        round <- round.validateHigherBetExist
      } yield round.doCall.switchNextBettingPlayer.tryComplete


    private def doRaise: BettingRound = {
      val highestBet: Int = betAmountsByPlayer.values.max
      copy(
        betAmountsByPlayer = betAmountsByPlayer + (currentBettingPlayer -> (highestBet + currentStake)))
    }

    def raise: Either[Error, BettingRound] =
      for {
        round <- validateIsNotCompleted
      } yield round.doRaise.switchNextBettingPlayer

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

    private def validateMinPlayerCount(players: List[Player])
                                      (implicit gameSettings: LimitHoldemSettings): ValidatedNec[Error, List[Player]] =
      if (players.length >= gameSettings.minPlayerCount)
        players.validNec
      else
        s"Error: ${gameSettings.minPlayerCount} or more players can play".error.invalidNec

    private def validateMaxPlayerCount(players: List[Player])
                                      (implicit gameSettings: LimitHoldemSettings): ValidatedNec[Error, List[Player]] =
      if (players.length <= gameSettings.maxPlayerCount)
        players.validNec
      else
        s"Error: ${gameSettings.maxPlayerCount} or less players can play".error.invalidNec

    def create(
                name: BettingRoundName,
                activePlayers: List[Player])
              (implicit gameSettings: LimitHoldemSettings):
    Either[Error, BettingRound] = {
      val activePlayersValidated = validateMinPlayerCount(activePlayers) *> validateMaxPlayerCount(activePlayers)

      activePlayersValidated.map { activePlayers =>
        val bets = activePlayers.map(_ -> 0).toMap
        new BettingRound(name, activePlayers, activePlayers.head, bets, false)
      }.toEither
        .leftMap(errs => Error(errs.map(_.message).mkString_(";")))
    }

    def createInitial(activePlayers: List[Player])
                     (implicit gameSettings: LimitHoldemSettings): Either[Error, BettingRound] =
      for {
        round <- create(BettingRoundName.Preflop, activePlayers)
      } yield round.makeBlinds
  }

}