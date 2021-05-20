package akovalevsky.scalabootcamp.poker.gameengine

import akovalevsky.scalabootcamp.poker.gameengine.Cards.{Card, Rank, Suit}

import scala.collection.SortedMap
import cats.syntax.all._

object HandEvaluator {

  sealed trait Combination

  object Combination {
    private val combinationsOrderedByValueAsc: List[Combination] =
      List(HighCard, Pair, TwoPairs, ThreeOfAKind, AceLowStraight,
        Straight, Flush, FullHouse, FourOfAKind, AceLowStraightFlush, StraightFlush)

    val values: Map[Combination, Int] = combinationsOrderedByValueAsc.zipWithIndex.toMap

    implicit val orderingForCombination: Ordering[Combination] =
      (x: Combination, y: Combination) => values(x) - values(y)

    case object StraightFlush extends Combination

    case object AceLowStraightFlush extends Combination

    case object FourOfAKind extends Combination

    case object FullHouse extends Combination

    case object Flush extends Combination

    case object Straight extends Combination

    case object AceLowStraight extends Combination

    case object ThreeOfAKind extends Combination

    case object TwoPairs extends Combination

    case object Pair extends Combination

    case object HighCard extends Combination

  }

  case class HandEvaluationResult(combination: Combination, score: Int)

  def pair(rankCounts: SortedMap[Rank, Int]): Option[Combination] =
    Option.when(rankCounts.values.count(_ == 2) == 1)(Combination.Pair)

  def twoPairs(rankCounts: SortedMap[Rank, Int]): Option[Combination] =
    Option.when(rankCounts.values.count(_ == 2) == 2)(Combination.TwoPairs)

  def threeOfAKind(rankCounts: SortedMap[Rank, Int]): Option[Combination] =
    Option.when(rankCounts.values.exists(_ == 3))(Combination.ThreeOfAKind)

  def aceLowStraight(rankCounts: SortedMap[Rank, Int]): Option[Combination] =
    Option.when(
      rankCounts(Rank.Ace) == 1 && rankCounts.values.takeWhile(_ == 1).toList.length == 4)(Combination.AceLowStraight)

  def straight(rankCounts: SortedMap[Rank, Int]): Option[Combination] =
    Option.when(rankCounts.values.dropWhile(_ != 1).takeWhile(_ == 1).toList.length == 5)(Combination.Straight)

  def flush(suitCounts: Map[Suit, Int]): Option[Combination] =
    Option.when(suitCounts.values.exists(_ == 5))(Combination.Flush)

  def fullHouse(rankCounts: SortedMap[Rank, Int]): Option[Combination] =
    for {
      _ <- pair(rankCounts)
      _ <- threeOfAKind(rankCounts)
    } yield Combination.FullHouse

  def fourOfAKind(rankCounts: SortedMap[Rank, Int]): Option[Combination] =
    Option.when(rankCounts.values.exists(_ == 4))(Combination.FourOfAKind)

  def aceLowStraightFlush(rankCounts: SortedMap[Rank, Int], suitCounts: Map[Suit, Int]): Option[Combination] =
    for {
      _ <- aceLowStraight(rankCounts)
      _ <- flush(suitCounts)
    } yield Combination.AceLowStraightFlush

  def straightFlush(rankCounts: SortedMap[Rank, Int], suitCounts: Map[Suit, Int]): Option[Combination] =
    for {
      _ <- straight(rankCounts)
      _ <- flush(suitCounts)
    } yield Combination.StraightFlush

  def evaluateHand(hand: List[Card]): HandEvaluationResult = {
    val rankCounts: Map[Rank, Int] =
      Rank.all.map(_ -> 0).toMap |+| hand.groupBy(_.rank).transform((_, cards) => cards.length)

    val rankCountsSorted: SortedMap[Rank, Int] = SortedMap.from(rankCounts)
    val suitCounts: Map[Suit, Int] = hand.groupBy(_.suit).transform((_, cards) => cards.length)

    val highestCombination = straightFlush(rankCountsSorted, suitCounts) orElse
      aceLowStraightFlush(rankCountsSorted, suitCounts) orElse
      fourOfAKind(rankCountsSorted) orElse
      fullHouse(rankCountsSorted) orElse
      flush(suitCounts) orElse
      straight(rankCountsSorted) orElse
      aceLowStraight(rankCountsSorted) orElse
      threeOfAKind(rankCountsSorted) orElse
      twoPairs(rankCountsSorted) orElse
      pair(rankCountsSorted) getOrElse Combination.HighCard

    val highestCombinationScore = Combination.values(highestCombination)

    def log2(x: Double): Double = math.log(x) / math.log(2)

    val bitsToEncodeRank = log2(Rank.values.values.max).floor.toInt
    val handCardsScore = hand
      .map(_.rank)
      .sortBy(rank => (rankCounts(rank), Rank.value(rank)))
      .map(rank => Rank.value(rank))
      .zipWithIndex
      .map { case (rank, idx) => rank << idx * bitsToEncodeRank }
      .sum

    val handScore = (highestCombinationScore << bitsToEncodeRank * 5) | handCardsScore

    HandEvaluationResult(highestCombination, handScore)
  }

  def main(args: Array[String]): Unit = {
    println(evaluateHand(List(
      Card(Rank.Ace, Suit.Clubs), Card(Rank.Queen, Suit.Clubs),
      Card(Rank.Jack, Suit.Clubs), Card(Rank.Ten, Suit.Clubs), Card(Rank.King, Suit.Clubs))))

    println(evaluateHand(List(
      Card(Rank.Ace, Suit.Clubs), Card(Rank.Two, Suit.Diamonds),
      Card(Rank.Three, Suit.Clubs), Card(Rank.Four, Suit.Hearts), Card(Rank.Five, Suit.Clubs))))

    println(evaluateHand(List(
      Card(Rank.Ace, Suit.Clubs), Card(Rank.Seven, Suit.Diamonds),
      Card(Rank.Three, Suit.Clubs), Card(Rank.Four, Suit.Hearts), Card(Rank.Five, Suit.Clubs))))
  }

}
