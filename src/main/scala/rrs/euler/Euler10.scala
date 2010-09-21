/*
	Euler10.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

	Find the sum of all the primes below two million.
*/

package		rrs.euler


/**
	class Euler10
*/
object		Euler10
{
	import rrs.num.PrimesBI._

	def
	euler10(limit: BigInt): BigInt =
		primeStream takeWhile (_ < limit) reduceLeft (_ + _)

	def
	main(args: Array[String]): Unit =
		printf("Euler10: %d%n", euler10(2000000))
}
