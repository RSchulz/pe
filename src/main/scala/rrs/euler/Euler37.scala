/*
	Euler37.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The number 3797 has an interesting property. Being prime itself,
	it is possible to continuously remove digits from left to right,
	and remain prime at each stage: 3797, 797, 97, and 7. Similarly
	we can work from right to left: 3797, 379, 37, and 3.

	Find the sum of the only eleven primes that are both truncatable
	from left to right and right to left.

	NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
*/


package		rrs.euler


/**
	class Euler37
*/
object		Euler37
{
	import rrs.num.Primality._

	def
	isBiTruncPrime(n: Long): Boolean =
		isPrime(n) && isRTruncPrime(n / 10) && isLTruncPrime(lTrunc(n))

	def
	isRTruncPrime(n: Long): Boolean =
		isPrime(n) && (n < 10 || isRTruncPrime(n / 10))

	def
	isLTruncPrime(n: Long): Boolean =
		isPrime(n) && (n < 10 || isLTruncPrime(lTrunc(n)))


	def
	lTrunc(n: Long): Long =
		n % mag(n)

	def
	mag(n: Long): Long = {
		var nn = math.abs(n)
		var l = 1l
		while (nn >= 10) {
			nn /= 10
			l *= 10
		}
		l
	}

	def
	ints(n: Long): Stream[Long] =
		n #:: ints(n + 1)


	def
	findBTPs(nBTPs: Int): Long =
		ints(11) filter (isBiTruncPrime(_)) take(nBTPs) sum


	def
	euler37: Long =
		findBTPs(11)

	def
	main(args: Array[String]): Unit =
		printf("Euler37: %d%n", euler37)
}
