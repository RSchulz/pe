/*
	Euler41.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	We shall say that an n-digit number is pandigital if
	it makes use of all the digits 1 to n exactly once.
	For example, 2143 is a 4-digit pandigital and is also prime.

	What is the largest n-digit pandigital prime that exists?
*/


package		rrs.euler


/**
	class Euler41
*/
object		Euler41
{
	import rrs.num._
	import Primality._
	import Permute._

	import collection.mutable.{Buffer, ArrayBuffer}


	def
	collectPDPs(n: Int, pdps: Buffer[Int]): Unit = {
		val perm = perm1(n)
		while (advancePerm(perm)) {
			val lPerm = pToInt(perm)
			if (isPrime(lPerm))
				pdps += lPerm
		}
	}

	def
	pToInt(p: Array[Int]): Int =
		(0 /: p)(_ * 10 + _)

//	def
//	findPDPs(n: Int): Seq[Int] =
//		permutations(n) map(pToInt(_)) filter(l => nDigits(l) == n && isPrime(l))
//
//	def
//	largestPDP(n: Int): Option[Int] = {
//		val pdps = findPDPs(n)
//		if (!pdps.isEmpty)
//			Some(pdps.last)
//		else
//			None
//	}

//	def
//	pToIntI(p: Array[Int]): Int = {
//		val n = p.length
//		var i = 0
//		var l = 0l
//		while (i < n) {
//			l = l * 10 + p(i)
//			i += 1
//		}
//		l
//	}

	def
	euler41: (Int, Int) = {
		val pdps = new ArrayBuffer[Int]
		for (n <- 2 until 10)
			collectPDPs(n, pdps)
		(pdps.length, pdps.max)
	}

	def
	main(args: Array[String]): Unit = {
		val answer = euler41
		printf("Euler41: |PDP|=%d; max(PDP)=%d%n", answer._1, answer._2)
	}
}
