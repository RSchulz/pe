/*
	Euler50.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The prime 41, can be written as the sum of six consecutive primes:
		41 = 2 + 3 + 5 + 7 + 11 + 13

	This is the longest sum of consecutive primes that adds to a prime below one-hundred.

	The longest sum of consecutive primes below one-thousand that adds to a prime,
	contains 21 terms, and is equal to 953.

	Which prime, below one-million, can be written as the sum of the most consecutive primes?
*/


package		rrs.euler


/**
	class Euler50
*/
object		Euler50
{
	import rrs.num.PrimesL._

	import collection.mutable.BitSet


	val limit	= 1e6.toLong
	val npEst	= (pi(limit) * 1.15).toInt
	val primes	= new Array[Long](npEst)
	val nPrimes	= primeLongs(limit, primes)
	val pBits	= BitSet((primes map(_.toInt)): _*)

	case
	class	AggregatePrime(p: Long, nthFirst: Int, nPrimes: Int)


	def
	maxAPrime(l: Int, h: Int, limit: Long): Option[AggregatePrime] = {
		var pSum = 0l
		var lastPrimeSum = 0l
		var lastPrimeEnd = 0
		var i = l
		while (i <= h && pSum < limit) {
			pSum += primes(i)
			if (isPrime(pSum)) {
				lastPrimeSum = pSum
				lastPrimeEnd = i
			}
			i += 1
		}
		if (lastPrimeEnd != l)
			Some(AggregatePrime(lastPrimeSum, l, lastPrimeEnd - l + 1))
		else
			None
	}

	def
	isPrime(n: Long): Boolean =
		n < limit && pBits(n.toInt)


	def
	euler50(limit: Long): AggregatePrime =
		(0 until nPrimes).flatMap(maxAPrime(_, nPrimes - 1, limit))
		 .max(Ordering.by((ap: AggregatePrime) => ap.nPrimes))

	def
	main(args: Array[String]): Unit = {
		val answer = euler50(limit)
		printf("Euler50: %d (%d primes starting at %d)%n", answer.p, answer.nPrimes, primes(answer.nthFirst))
	}
}
