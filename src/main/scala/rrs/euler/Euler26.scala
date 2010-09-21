/*
	Euler26.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	A unit fraction contains 1 in the numerator.
	The decimal representation of the unit fractions with denominators 2 to 10 are given:

		1/2		= 	0.5
		1/3		= 	0.(3)
		1/4		= 	0.25
		1/5		= 	0.2
		1/6		= 	0.1(6)
		1/7		= 	0.(142857)
		1/8		= 	0.125
		1/9		= 	0.(1)
		1/10	= 	0.1

	Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle.
	It can be seen that ^(1)/_(7) has a 6-digit recurring cycle.

	Find the value of d < 1000 for which ^(1)/_(d) contains the
	longest recurring cycle in its decimal fraction part.
*/

package		rrs.euler


/**
	class Euler26
*/
object		Euler26
{
	import rrs.num.Factorization

	def
	isTerminating10(num: Int, denom: Int): Boolean = {
		val dFact = Factorization(denom)
		dFact.factors forall { factor => factor.p == 2 || factor.p == 5 }
	}

	def
	denomM10(denom: Int): BigInt = {
		var M = BigInt(denom)
		while (M % 2 == 0) M /= 2
		while (M % 5 == 0) M /= 5
		M
	}

	def
	lDiv10(M: BigInt): BigInt = {
		var (divisor, nNines) = nines(M)
		var rem = BigInt(0)

		do {
			rem = divisor % M
			if (rem != 0) {
				val more = nines(M, rem)
				divisor = more._1
				nNines += more._2
			}
		}
		while (rem != 0)

		nNines
	}

	def
	repSize10(d: Int): BigInt =
		lDiv10(denomM10(d))

	def
	nines(M: BigInt, r: BigInt): (BigInt, Int) = {
		var n = 0
		var rr = r
		while (rr < M) {
			rr = rr * 10 + 9
			n += 1
		}
		(rr, n)
	}

	def
	nines(M: BigInt): (BigInt, BigInt) = {
		var n = BigInt(1)
		var n9s = BigInt(9)
		while (n9s < M) {
			n9s = n9s * 10 + 9
			n += 1
		}
		(n9s, n)
	}

	def
	euler26: (Int, BigInt) =
		(1 until 1000)
		 .filter(!isTerminating10(1, _))
		 .map(d => (d, repSize10(d)))
		 .max(Ordering.by((dr: (Int, BigInt)) => dr._2))

	def
	main(args: Array[String]): Unit = {
		val answer = euler26
		printf("Euler26: 1/%d; %s%n", answer._1, answer._2)
	}
}
