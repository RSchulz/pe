/*
	Euler54.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	In the card game poker, a hand consists of five cards and are
	ranked, from lowest to highest, in the following way:

		* High Card: Highest value card.
		* One Pair: Two cards of the same value.
		* Two Pairs: Two different pairs.
		* Three of a Kind: Three cards of the same value.
		* Straight: All cards are consecutive values.
		* Flush: All cards of the same suit.
		* Full House: Three of a kind and a pair.
		* Four of a Kind: Four cards of the same value.
		* Straight Flush: All cards are consecutive values of same suit.
		* Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

	The cards are valued in the order:

		2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

	If two players have the same ranked hands then the rank made up
	of the highest value wins; for example, a pair of eights beats a
	pair of fives (see example 1 below). But if two ranks tie, for example,
	both players have a pair of queens, then highest cards in each hand
	are compared (see example 4 below); if the highest cards tie then
	the next highest cards are compared, and so on.

	Consider the following five hands dealt to two players:
		Hand		Player 1			Player 2				Winner

		1			5H 5C 6S 7S KD		2C 3S 8S 8D TD
					Pair of Fives		Pair of Eights			Player 2

		2			5D 8C 9S JS AC		2C 5C 7D 8S QH
					Highest card Ace	Highest card Queen		Player 1

		3			2D 9C AS AH AC		3D 6D 7D TD QD
					Three Aces			Flush with Diamonds		Player 2

		4			4D 6S 9H QH QC		3D 6D 7H QD QS
					Pair of Queens		Pair of Queens
					Highest card Nine	Highest card Seven		Player 1

		5			2H 2D 4C 4D 4S		3C 3D 3S 9S 9D
					Full House			Full House
					With Three Fours	With Three Threes		Player 1


	The file, poker.txt (<http://projecteuler.net/project/poker.txt>),
	contains one-thousand random hands dealt to two players.
	Each line of the file contains ten cards (separated by a single space):
	the first five are Player 1's cards and the last five are Player 2's cards.
	You can assume that all hands are valid (no invalid characters or repeated cards),
	each player's hand is in no specific order, and in each hand there is a clear winner.

	How many hands does Player 1 win?
*/

package		rrs.euler


/**
	class Euler54
*/
object		Euler54
{
	import	io.Source
	import	collection.mutable.ArrayBuffer


	abstract
	class		Suit(val symbol: Char, val order: Int)

	case object Hearts		extends Suit('H', 1)
	case object Clubs		extends Suit('C', 2)
	case object Spades		extends Suit('S', 3)
	case object Diamonds	extends Suit('D', 4)


	abstract
	class		Rank(val symbol: Char, val order: Int)

	case object R2			extends Rank('2', 1)
	case object R3			extends Rank('3', 2)
	case object R4			extends Rank('4', 3)
	case object R5			extends Rank('5', 4)
	case object R6			extends Rank('6', 5)
	case object R7			extends Rank('7', 6)
	case object R8			extends Rank('8', 7)
	case object R9			extends Rank('9', 8)
	case object RT			extends Rank('T', 9)
	case object Jack		extends Rank('J', 10)
	case object Queen		extends Rank('Q', 11)
	case object King		extends Rank('K', 12)
	case object Ace			extends Rank('A', 13)


	abstract
	class		PokerRank(val name: String, val order: Int)

	case object HighCard		extends PokerRank("high card",		1)
	case object OnePair			extends PokerRank("one pair",		2)
	case object TwoPair			extends PokerRank("two pair",		3)
	case object ThreeKind		extends PokerRank("three kind",		4)
	case object Straight		extends PokerRank("straight",		5)
	case object Flush			extends PokerRank("flush",			6)
	case object FullHouse		extends PokerRank("full house",		7)
	case object FourKind		extends PokerRank("four kind",		8)
	case object StraightFlush	extends PokerRank("straight flush",	9)
	case object RoyalFlush		extends PokerRank("royal flush",	10)


	case
	class	Card(suit: Suit, rank: Rank)
	{
		override
		def
		toString: String =
			"%c%c".format(rank.symbol, suit.symbol)
	}


	val
	cardOrderingRank =
		new Ordering[Card] {
			def
			compare(a: Card, b: Card): Int = {
				val ab = a.rank.order - b.rank.order
				if (ab != 0)
					ab
				else
					a.suit.order - b.suit.order
			}
		}

//	val
//	cardOrderingSuit =
//		new Ordering[Card] {
//			def
//			compare(a: Card, b: Card): Int = {
//				val ab = a.suit.order - b.suit.order
//				if (ab != 0)
//					ab
//				else
//					a.rank.order - b.rank.order
//			}
//		}


	val
	rankOrdering =
		new Ordering[Rank] {
			def
			compare(a: Rank, b: Rank): Int =
				a.order - b.order
		}


	abstract
	class	Hand
	{
		def
		cards: Seq[Card]

		override
		def
		toString: String =
			"« %s »".format(cards.mkString(" "))
	}

	case
	class	PokerHand(cards: IndexedSeq[Card])
	extends	Hand
	{
		require(cards.length == 5, "Poker hand must have exactly 5 cards; %d given".format(cards.length))

		val grouped = cards.groupBy(_.rank)

		def
		singles: Iterable[Rank] =
			grouped filter(_._2.length == 1) map(_._1)

		def
		pairs: Iterable[Rank] =
			grouped filter(_._2.length == 2) map(_._1)

		def
		threes: Iterable[Rank] =
			grouped filter(_._2.length == 3) map(_._1)

		def
		fours: Iterable[Rank] =
			grouped filter(_._2.length == 4) map(_._1)

		def
		highCard: Card =
			cards(4)

		def
		highestFirst: Stream[Rank] =
			(for (i <- 4 to 0 by -1) yield cards(i).rank).toStream


		def
		beats(his: PokerHand): Boolean = {
			val me = classify
			val him = his.classify

			if (me != him)
				me.order > him.order

			else {
				me match {
					case OnePair | TwoPair =>
						val myHigh = pairs.max(rankOrdering)
						val hisHigh = his.pairs.max(rankOrdering)

						if (myHigh == hisHigh)
							isHigherThan(his)
						else
							myHigh.order > hisHigh.order

					case ThreeKind | FullHouse =>
						val myHigh = threes.max(rankOrdering)
						val hisHigh = threes.max(rankOrdering)

						if (myHigh == hisHigh)
							isHigherThan(his)
						else
							myHigh.order > hisHigh.order

					case _ => isHigherThan(his)
				}
			}
		}


		def
		isHigherThan(his: PokerHand): Boolean = {
			val paired = (highestFirst zip his.highestFirst) dropWhile(hp => hp._1.order == hp._2.order)
			!paired.isEmpty && paired.head._1.order > paired.head._2.order
		}


		def
		classify: PokerRank = {
				 if (isRoyalFlush)		RoyalFlush
			else if (isStraightFlush)	StraightFlush
			else if (isFourKind)		FourKind
			else if (isFullHouse)		FullHouse
			else if (isFlush)			Flush
			else if (isStraight)		Straight
			else if (isThreeKind)		ThreeKind
			else if (isTwoPair)			TwoPair
			else if (isOnePair)			OnePair
			else						HighCard
		}

		def
		isRoyalFlush: Boolean =
			isStraightFlush && highCard.rank == Ace

		def
		isStraightFlush: Boolean =
			isStraight && isFlush

		def
		isFourKind: Boolean =
			grouped.values.exists(_.size == 4)

		def
		isFullHouse: Boolean =
			isThreeKind && isOnePair

		def
		isFlush: Boolean =
			cards forall(_.suit == cards(0).suit)

		def
		isStraight: Boolean =
			(cards(4).rank.order) - (cards(0).rank.order) == 4 && (grouped.size == 5)

		def
		isThreeKind: Boolean =
			grouped.values.exists(_.size == 3)

		def
		isTwoPair: Boolean =
			(grouped.values.count(_.size == 2)) == 2

		def
		isOnePair: Boolean =
			(grouped.values.count(_.size == 2)) == 1
	}


	def
	card(designation: String): Card = {
		require(designation.length == 2,
				"Card designation must contain exactly two characters; %d given".format(designation.length))
		Card(suit(designation(1)), rank(designation(0)))
	}

	def
	rank(code: Char): Rank =
		code match {
			case '2' => R2
			case '3' => R3
			case '4' => R4
			case '5' => R5
			case '6' => R6
			case '7' => R7
			case '8' => R8
			case '9' => R9
			case 'T' => RT
			case 'J' => Jack
			case 'Q' => Queen
			case 'K' => King
			case 'A' => Ace
		}

	def
	suit(code: Char): Suit =
		code match {
			case 'H' => Hearts
			case 'C' => Clubs
			case 'S' => Spades
			case 'D' => Diamonds
		}

	def
	pokerHand(designations: IndexedSeq[String]): PokerHand =
		PokerHand(designations.map(card(_)).sorted(cardOrderingRank))


	def
	readHands(handsFile: String): Seq[(PokerHand, PokerHand)] = {
		val handsSource = Source.fromFile(handsFile)
		val handPairs = new ArrayBuffer[(PokerHand, PokerHand)]
		val lineSplit = "\\s+"

		handsSource.getLines.foreach { line =>
										val designations = line.split(" +")
										assert(designations.length == 10, "Malformed input")
										handPairs += ((pokerHand(designations.slice(0, 5)),
													   pokerHand(designations.slice(5, 10))))
									 }
		handsSource.close
		handPairs
	}


	def
	euler54(hands: Seq[(PokerHand, PokerHand)], verbose: Boolean): (Int, Int) = {
		var p1Score = 0
		var p2Score = 0

		hands foreach { hp =>
			val hand1 = hp._1
			val hand2 = hp._2
			val h1Wins = hand1.beats(hand2)

			if (verbose)
				printf("P1: %s (%s) %s%nP2: %s (%s) %s%n%n",
					   hand1, hand1.classify, if (h1Wins) "wins" else "",
					   hand2, hand2.classify, if (h1Wins) "" else "wins")

			if (h1Wins)
				p1Score += 1
			else
				p2Score += 1
		}

		(p1Score, p2Score)
	}

	def
	main(args: Array[String]): Unit = {
		var verbose = false
		args foreach {
			case "-v" => verbose = true
			case handsFile =>
				val hands = readHands(handsFile)
				val answer = euler54(hands, verbose)
				printf("Euler54: \"%s\"; player 1: %d; player 2: %d%n", handsFile, answer._1, answer._2)
		}
	}
}
