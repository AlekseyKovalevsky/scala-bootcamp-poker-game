package akovalevsky.scalabootcamp.poker.gameengine

import akovalevsky.scalabootcamp.poker.gameengine.Cards.Card._
import akovalevsky.scalabootcamp.poker.gameengine.Cards.Rank._
import akovalevsky.scalabootcamp.poker.gameengine.Cards.Suit._
import akovalevsky.scalabootcamp.poker.gameengine.Common._
import cats.effect.Sync
import cats.syntax.all._

object Cards {

  sealed trait Suit

  object Suit {

    case object Diamonds extends Suit

    case object Clubs extends Suit

    case object Hearts extends Suit

    case object Spades extends Suit

    val all: List[Suit] = List(Diamonds, Clubs, Hearts, Spades)
  }

  sealed trait Rank

  object Rank {
    private val ranksOrderedByValueAsc: List[Rank] = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)

    val values: Map[Rank, Int] = ranksOrderedByValueAsc.zipWithIndex.toMap

    def value(rank: Rank): Int = values(rank)

    implicit val orderingForRank: Ordering[Rank] = (x: Rank, y: Rank) => values(x) - values(y)

    case object Ace extends Rank

    case object King extends Rank

    case object Queen extends Rank

    case object Jack extends Rank

    case object Ten extends Rank

    case object Nine extends Rank

    case object Eight extends Rank

    case object Seven extends Rank

    case object Six extends Rank

    case object Five extends Rank

    case object Four extends Rank

    case object Three extends Rank

    case object Two extends Rank

    val all: List[Rank] = List(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two)
  }

  final case class Card(rank: Rank, suit: Suit) {
    override def toString: String = rankStr(rank) + suitStr(suit)
  }

  object Card {
    implicit val orderingForCard: Ordering[Card] = Ordering[Rank].contramap[Card](_.rank)

    private val suitStr = Map[Suit, String](
      Diamonds -> "d",
      Hearts -> "h",
      Spades -> "s",
      Clubs -> "c"
    )

    private val rankStr = Map[Rank, String](
      Ace -> "A",
      King -> "K",
      Queen -> "Q",
      Jack -> "J",
      Ten -> "T",
      Nine -> "9",
      Eight -> "8",
      Seven -> "7",
      Six -> "6",
      Five -> "5",
      Four -> "4",
      Three -> "3",
      Two -> "3"
    )

    def toString(card: Card): String = rankStr(card.rank) + suitStr(card.suit)
  }

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
    def shuffled[F[_] : Sync](shuffle: List[Card] => F[List[Card]]): F[Deck] =
      for {
        cards <- Sync[F].delay {
          for {
            rank <- Rank.all
            suit <- Suit.all
          } yield Card(rank, suit)
        }
        cardsShuffled <- shuffle(cards)
      } yield new Deck(cardsShuffled)
  }

}

