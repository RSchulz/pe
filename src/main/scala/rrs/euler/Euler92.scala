/*
	Euler92.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	A number chain is created by continuously adding the square of the
	digits in a number to form a new number until it has been seen before.

	For example,

		44 → 32 → 13 → 10 → 1 → 1
		85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

	Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop.
	What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

	How many starting numbers below ten million will arrive at 89?
*/


package		rrs.euler


/**
	object Euler92
*/
object		Euler92
{
	def
	next(i: Int): Int =
		digits(i) map(d => d * d) sum

	def
	digits(i: Int): Seq[Int] =
		i.toString.map(_ - '0')

	def
	chain(i: Int): Stream[Int] =
		i #:: chain(next(i))


	def
	terminus(i: Int): Int =
		chain(i) dropWhile(n => n != 1 && n != 89) head


	def
	euler92: Int =
		(1 to 10000000) count(terminus(_) == 89)

	def
	main(args: Array[String]): Unit =
		printf("Euler92: %d%n", euler92)
}
