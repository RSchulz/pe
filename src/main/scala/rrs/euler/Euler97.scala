/*
	Euler97.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The first known prime found to exceed one million digits
	was discovered in 1999, and is a Mersenne prime of the form
	2^6972593 − 1; it contains exactly 2,098,960 digits.
	Subsequently other Mersenne primes, of the form 2^p − 1,
	have been found which contain more digits.

	However, in 2004 there was found a massive non-Mersenne prime
	which contains 2,357,207 digits: 28433 × 2^7830457 + 1.

	Find the last ten digits of this prime number.
*/

package		rrs.euler


/**
	object Euler97
*/
object		Euler97
{
	def
	euler97: Long = {
		var l = 1l

		for (n <- 1 to 7830457) {
			l *= 2
			if (l > 1e11.toLong)
				l -= 1e11.toLong
		}

		l *= 28433
		l += 1

		l % 1e10.toLong
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler97: %d%n", euler97)
}
