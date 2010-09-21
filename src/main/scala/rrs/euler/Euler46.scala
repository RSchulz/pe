/*
	Euler46.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	It was proposed by Christian Goldbach that every odd composite
	number can be written as the sum of a prime and twice a square.

		 9	=  7 + 2×1^2
		15	=  7 + 2×2^2
		21	=  3 + 2×3^2
		25	=  7 + 2×3^2
		27	= 19 + 2×2^2
		33	= 31 + 2×1^2

	It turns out that the conjecture was false.

	What is the smallest odd composite that cannot be
	written as the sum of a prime and twice a square?
*/


package		rrs.euler


/**
	class Euler46
*/
object		Euler46
{
	import rrs.num.PrimesL._

	import collection.mutable.BitSet
	import math._


	val limit	= 1e5.toLong
	val npEst	= (pi(limit) * 1.15).toInt
	val primes	= new Array[Long](npEst)
	val nPrimes	= primeLongs(limit, primes)
	val pBits	= BitSet((primes map(_.toInt)): _*)


	def
	isPp2S(o: Int): Boolean =
	(	!pBits(o)
	&&	(((o - 2) to 2 by -1) exists(p => pBits(p) && ((p to 1 by -1) exists(j => isSquare((o - p) / 2))))) )


	def
	isSquare(x: Int): Boolean = {
		val r = ceil(sqrt(x)).toInt
		r * r == x
	}

	def
	ints(n: Int): Stream[Int] =
		n #:: ints(n + 1)


	def
	euler46: Int =
		ints(9) dropWhile( i => pBits(i) || i % 2 == 0 || isPp2S(i)) head

	def
	main(args: Array[String]): Unit =
		printf("Euler46: %d%n", euler46)
}
