/*
	Euler27.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	Euler published the remarkable quadratic formula:

	n² + n + 41

	It turns out that the formula will produce 40 primes for the consecutive values
	n = 0 to 39. However, when n = 40, 40^(2) + 40 + 41 = 40(40 + 1) + 41 is divisible by 41,
	and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

	Using computers, the incredible formula  n² − 79n + 1601 was discovered,
	which produces 80 primes for the consecutive values n = 0 to 79.
	The product of the coefficients, −79 and 1601, is −126479.

	Considering quadratics of the form:

		n² + an + b, where |a| < 1000 and |b| < 1000

		where |n| is the modulus/absolute value of n
		e.g. |11| = 11 and |−4| = 4

	Find the product of the coefficients, a and b, for the quadratic expression that
	produces the maximum number of primes for consecutive values of n, starting with n = 0.
*/

package		rrs.euler


/**
	class Euler27
*/
object		Euler27
{
	import math._
	import rrs.num._
	import PrimesL._

	/*
		b values of QuadAB that yield a prime for n = 0 must be prime
	*/

	val limit			= 1000

	val nPLongs1000		= 168
	val pLongs1000		= new Array[Long](nPLongs1000)
						  primeLongs(limit, pLongs1000)

	def
	isPrime(n: Int): Boolean =
		if (n < 0)
			return false
		else
		if (n == 2)
			return true
		else {
			var i = 0
			val lastFactor = n / 2 + 1
			while (i < nPLongs1000) {
				val p_i = pLongs1000(i)
				if (p_i >= lastFactor)
					return true
				else
				if (n % p_i == 0)
					return false
				else
					i += 1
			}
			return true
		}

	case
	class	QuadAB(a: Int, b: Int) {
		def
		apply(n: Int): Int =
			n * (n + a) + b

		override
		def
		toString: String =
			(a, b) match {
				case (0, 0) => "n^2"
				case (0, _) => "n^2 + b"
				case (1, 0) => "n^2 + n"
				case (1, _) => "n^2 + n + %d".format(b)
				case (_, 0) => "n^2 + %dn".format(a)
				case _      => "n^2 + %dn + %d".format(a, b)
			}
	}

	def
	countPrimes(qf: QuadAB): Int =
		if (qf.b == 0)
			0
		else
			(0 to abs(qf.b) takeWhile { n => isPrime(qf(n)) }) length

	def
	findMaxQuad: (QuadAB, Int) = {
		(for {
			b   <- -999 to 999
//			b_i <- -167 to 167
			a   <- -999 to 999
		} yield {
//			val bb = pLongs1000(abs(b_i)).toInt
//			val b = if (b_i < 0) -bb else bb
			val qf = QuadAB(a, b)
			(qf, countPrimes(qf))
		}) max(Ordering.by((qm: (QuadAB, Int)) => qm._2))
	}

	def
	euler27: (QuadAB, Int) =
		findMaxQuad

	def
	main(args: Array[String]): Unit = {
		val qabMax = euler27
		printf("Euler27: %s; a*b=%d%n", qabMax._1, qabMax._1.a * qabMax._1.b)
	}
}
