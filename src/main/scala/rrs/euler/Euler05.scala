/*
	Euler05.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	2520 is the smallest number that can be divided by
	each of the numbers from 1 to 10 without any remainder.

	What is the smallest positive number that is evenly
	divisible by all of the numbers from 1 to 20?
*/


package		rrs.euler

object		Euler05
{
	import rrs.num.PrimesL.primeBits
	import collection.mutable.HashMap
	import scala.math.max

	type Factorization = HashMap[Int, Int]

	def
	mkFactorization =
		new Factorization

	def
	iPow(base: Int, power: Int): Int =
		(1 /: List.fill(power)(base)) (_ * _)

	/** Compute the histogram factorizaton of <i>n</i> */
	def
	hFactors(n: Int): Factorization = {
		var residue = n
		val fhMap = mkFactorization
		primeBits(n) filter (n % _ == 0) foreach { prime =>
			var nFactors = 0
			while (residue % prime == 0) {
				nFactors += 1
				residue /= prime
			}
			if (nFactors > 0)
				fhMap(prime) = nFactors
		}
		fhMap
	}

	/**
		Compute the histogram factorization of the LCM of the numbers
		represented by the histogram factorizations <i>h1</i> and <i>h2</i>
	*/
	def
	lcmHFactor(h1: Factorization, h2: Factorization): Factorization = {
		val maxFactors = mkFactorization
		val factors = h1.keySet ++ h2.keySet
		factors.foreach { factor =>
			maxFactors(factor) = max(h1.getOrElse(factor, 0), h2.getOrElse(factor, 0))
		}
		maxFactors
	}

	/** Compute the histogram factorization of the LCM of all the numbers in <i>ns</i> */
	def
	lcmAll(ns: Seq[Int]): Factorization =
		(mkFactorization /: ns) { case (f, i) => lcmHFactor(f, hFactors(i)) }


	/** Compute the integer represented by the histogram factorization <i>hf</i> */
	def
	unfactor(hf: Factorization): Int =
		(1 /: hf) { (p: Int, f: (Int, Int)) => p * iPow(f._1, f._2) }


	/** Compute the number that is divided by all the integers in <i>ns</i> */
	def
	dividesAll(ns: Seq[Int]): Int =
		unfactor(lcmAll(ns))


	def
	main(args: Array[String]): Unit =
		if (args.length == 0)
			printf("Euler05: %d%n", dividesAll(1 to 20))
		else
			args.foreach { arg =>
				val iArg = arg.toInt
				printf("Euler05: %d: %d%n", iArg, dividesAll(1 to iArg))
			}
}

//	/** Compute the prime factors of <i>n</i> */
//	def
//	factor(n: Long): Seq[Long] = {
//		var latestFactor = n
//		val factors = new ArrayBuffer[Long]
//
//		if (n == 1l)
//			factors += 1l
//
//		else
//			while (latestFactor != 1l) {
//				var f = 2l
//				while (latestFactor % f != 0l)
//					f += 1l
//
//				factors += f
//				latestFactor /= f
//			}
//
//		factors
//	}

//	/** Compute the histogram factorizaton of <i>n</i> */
//	def
//	hFactors(n: Long): Factorization = {
//		val fhMap = mkFactorization
//		factor(n).foreach { factor =>
//			fhMap.get(factor) match {
//				case Some(c) => fhMap(factor) = c + 1
//				case None => fhMap(factor) = 1
//			}
//		}
//		fhMap
//	}

//	{
//		var n = 1
//		hf.foreach { case (factor, rep) =>
//			n *= iPow(factor, rep)
//		}
//		n
//	}

//	/** Compute the prime factors of <i>n</i> */
//	def
//	primes(n: Int): Seq[Int] =
//		PrimesL.primeBits(n).toSeq
