package akovalevsky.scalabootcamp.poker.gameengine

import akovalevsky.scalabootcamp.poker.gameengine.Common._

import scala.util.Random

object Cards {

  sealed trait Suit

  object Suit {

    final case object Diamonds extends Suit

    final case object Clubs extends Suit

    final case object Hearts extends Suit

    final case object Spades extends Suit

    val all: List[Suit] = List(Diamonds, Clubs, Hearts, Spades)
  }

  sealed trait Rank

  object Rank {
    private val ranksOrderedByValueAsc: List[Rank] = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)

    val values: Map[Rank, Int] = ranksOrderedByValueAsc.zipWithIndex.toMap

    def value(rank: Rank): Int = values(rank)

    implicit val orderingForRank: Ordering[Rank] = (x: Rank, y: Rank) => values(x) - values(y)

    final case object Ace extends Rank

    final case object King extends Rank

    final case object Queen extends Rank

    final case object Jack extends Rank

    final case object Ten extends Rank

    final case object Nine extends Rank

    final case object Eight extends Rank

    final case object Seven extends Rank

    final case object Six extends Rank

    final case object Five extends Rank

    final case object Four extends Rank

    final case object Three extends Rank

    final case object Two extends Rank

    val all: List[Rank] = List(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two)
  }

  final case class Card(rank: Rank, suit: Suit)

  final case class Hand private(cards: List[Card])

  object Hand {
    def create(cards: List[Card]): Either[Error, Hand] =
      if (cards.length == 5)
        Right(new Hand(cards))
      else
        Left("Hand must contain 5 cards.".error)
  }

  final case class Pocket private(cards: List[Card])

  object Pocket {
    def create(cards: List[Card]): Either[Error, Pocket] =
      if (cards.length == 2)
        Right(new Pocket(cards))
      else
        Left("Pocket must contain 2 cards.".error)
  }

  final case class Board private(cards: List[Card])

  object Board {
    def create(cards: List[Card]): Either[Error, Board] =
      if (cards.length >= 3 && cards.length <= 5)
        Right(new Board(cards))
      else
        Left("Board must contain 3 to 5 cards.".error)
  }

  case class Deck private(cards: List[Card])

  object Deck {
    def shuffle: Deck = {
      val cards = for {
        rank <- Rank.all
        suit <- Suit.all
      } yield Card(rank, suit)

      new Deck(Random.shuffle(cards))
    }
  }

}
