/*
	Euler14.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The following iterative sequence is defined for the set of positive integers:

	n → n/2 (n is even)
	n → 3n + 1 (n is odd)

	Using the rule above and starting with 13, we generate the following sequence:
	13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

	It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
	Although it has not been proved yet (Collatz Problem),
	it is thought that all starting numbers finish at 1.

	Which starting number, under one million, produces the longest chain?

	NOTE: Once the chain starts the terms are allowed to go above one million.
*/


package		rrs.euler

// Bugged: Ints overflow
object		Euler14I
{
	def
	e14SeqLen(s: Int): Int = {
		var sv = s
		var n = 0
		while (sv > 1) {
			sv =
				if (sv % 2 == 0)
					sv / 2
				else
					sv * 3 + 1
			n += 1
		}
		n
	}

	def
	findLongest: (Int, Int) = {
		var longest = 0
		var longestLength = 0
		for (i <- 1 until 1000000) {
			val length = e14SeqLen(i)
			if (length > longestLength) {
				longest = i
				longestLength = length
			}
		}

		printf("Euler14: start=%d; length=%d%n", longest, longestLength)

		(longest, longestLength)
	}

	def
	main(args: Array[String]): Unit =
		findLongest
}


object		Euler14L
{
	def
	e14SeqLen(s: Long): Long = {
		var sv = s
		var n: Long = 0
		while (sv > 1) {
			sv =
				if (sv % 2 == 0)
					sv / 2
				else
					sv * 3 + 1
			n += 1
		}
		n
	}

	def
	findLongest: (Long, Long) = {
		var longest: Long = 0
		var longestLength: Long = 0
		for (i <- 1 until 1000000) {
			val length = e14SeqLen(i)
			if (length > longestLength) {
				longest = i
				longestLength = length
			}
		}

		printf("Euler14: start=%d; length=%d%n", longest, longestLength)

		(longest, longestLength)
	}

	def
	main(args: Array[String]): Unit =
		findLongest
}


object		Euler14BI
{
	def
	e14SeqLen(s: Int): BigInt = {
		var sv = BigInt(s)
		var n = 0
		while (sv > 1) {
			sv =
				if (sv % 2 == 0)
					sv / 2
				else
					sv * 3 + 1
			n += 1
		}
		n
	}

	def
	findLongest: (BigInt, BigInt) = {
		var longest = BigInt(0)
		var longestLength = BigInt(0)
		for (i <- 1 until 1000000) {
			val length = e14SeqLen(i)
			if (length > longestLength) {
				longest = i
				longestLength = length
			}
		}

		printf("Euler14: start=%d; length=%d%n", longest.intValue, longestLength.intValue)

		(longest, longestLength)
	}

	def
	main(args: Array[String]): Unit =
		findLongest
}
