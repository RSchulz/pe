/*
	Euler06.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The sum of the squares of the first ten natural numbers is,
		1^(2) + 2^(2) + ... + 10^(2) = 385

	The square of the sum of the first ten natural numbers is,
		(1 + 2 + ... + 10)^(2) = 55^(2) = 3025

	Hence the difference between the sum of the squares of the first ten natural numbers
		and the square of the sum is 3025 − 385 = 2640.

	Find the difference between the sum of the squares of the first one hundred natural numbers
		and the square of the[ir] sum.
*/

package		rrs.euler

import		scala.collection.immutable.NumericRange


/**
	class Euler06
*/
object		Euler06
{
	def
	sumSquares(limit: Long): Long =
		(0L /: NumericRange(1, limit + 1, 1))((sum: Long, i: Long) => sum + i * i)

	def
	sum(limit: Long): Long =
		(0L /: NumericRange(1, limit + 1, 1))(_ + _)

	def
	squareSum(limit: Long): Long = {
		val s = sum(limit)
		s * s
	}

	def
	euler06(limit: Long): Long =
		squareSum(limit) - sumSquares(limit)

	def
	main(args: Array[String]): Unit =
		printf("Euler06: %d%n", euler06(100))
}
