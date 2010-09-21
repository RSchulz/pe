/*
	Euler49.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The arithmetic sequence, 1487, 4817, 8147, in which each
	of the terms increases by 3330, is unusual in two ways:

		(i)  each of the three terms are prime, and,
		(ii) each of the 4-digit numbers are permutations of one another.

	There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
	exhibiting this property, but there is one other 4-digit increasing sequence.

	What 12-digit number do you form by concatenating the three terms in this sequence?
*/


package		rrs.euler


/**
	class Euler49
*/
object		Euler49
{
	import rrs.num.PrimesL._
	import rrs.num.Permute._

	import collection.mutable.BitSet
	import util.Sorting.{quickSort => sort}


	val limit		= 1e4.toInt
	val npEst		= (pi(limit) * 1.15).toInt
	val lPrimes		= new Array[Long](npEst)
	val nPrimes		= primeLongs(limit, lPrimes)
	val primes		= Array[Int]((for (i <- 0 until nPrimes) yield lPrimes(i).toInt): _*)
	val isPrime		= BitSet(primes: _*)


	def
	primePermutations(p: Int, nDigits: Int): Array[Int] = {
		val pDigits = toDigits(p)
		( ( permutations(nDigits)
			.flatMap { perm =>
				val permuted = permute(pDigits, perm)
				val pPrime = fromDigits(permuted)
				if (permuted(0) != 0 && isPrime(pPrime))
					Some(pPrime)
				else
					None
			}
		) ).distinct.toArray
	}

	def
	toDigits(l: Int): Array[Int] =
		l.toString.map(_ - '0').toArray

	def
	fromDigits(digits: Array[Int]): Int =
		(0 /: digits)(_ * 10 + _)

	def
	permute(digits: Array[Int], permutation: Array[Int]): Array[Int] =
		((0 until digits.length) map(i => digits(permutation(i)))).toArray


	def
	equiDelta(ints: Array[Int], nDeltas: Int): Seq[(Int, Seq[Int])] =
		if (ints.length < nDeltas + 1)
			Seq()
		else {
			sort(ints)
			val nInts = ints.length

			( ( for {	i <- 0 until nInts - 1
						j <- i + 1 until nInts
				} yield	(i, j, ints(j) - ints(i))
			  ).groupBy(_._3)
			   .filter(dGroup => dGroup._2.length >= nDeltas && chained(dGroup._2: _*))
			   .map { dg =>
					val delta = dg._1
					val first = ints(dg._2(0)._1)
					(delta, Array.range(first, first + delta * nDeltas + 1, delta).toSeq)
				}
			).toSeq
		}

	def
	chained(links: (Int, Int, Int)*): Boolean =
		links sliding(2) forall(w => w(0)._2 == w(1)._1)



	def
	euler49: Unit = {
		val answers = ( primes
						.filter(_ > 1000)
						.map(primePermutations(_, 4))
						.flatMap(equiDelta(_, 2))
					  ) distinct

		answers foreach( a => printf("  %s: %s; %s%n", a._1, a._2.mkString("[", ", ", "]"), a._2.mkString("")) )
	}

	def
	main(args: Array[String]): Unit = {
		println("Euler49:")
		euler49
	}
}
