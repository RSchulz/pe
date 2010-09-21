/*
	Euler47.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The first two consecutive numbers to have two distinct prime factors are:

		14 = 2 × 7
		15 = 3 × 5

	The first three consecutive numbers to have three distinct prime factors are:

		644 = 2² × 7 × 23
		645 = 3 × 5 × 43
		646 = 2 × 17 × 19.

	Find the first four consecutive integers to have four distinct primes factors.
	What is the first of these numbers?
*/


package		rrs.euler


/**
	class Euler47
*/
object		Euler47
{
	import rrs.num.Factorization

	def
	fribble(n: Int): Long =
		(ints(4)
		 .map(Factorization(_))
		 .sliding(n)
		 .dropWhile(f => f.exists(_.factors.length != n))
		 .next
		 .head).n


	def
	ints(n: Long): Stream[Long] =
		n #:: ints(n + 1)

	def
	euler47: Long =
		fribble(4)

	def
	main(args: Array[String]): Unit =
		printf("Euler47: %d%n", euler47)
}
