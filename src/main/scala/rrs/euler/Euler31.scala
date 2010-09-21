/*
	Euler31.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	In England the currency is made up of pound, £, and pence, p,
	and there are eight coins in general circulation:

		1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

	It is possible to make £2 in the following way:

		1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

	How many different ways can £2 be made using any number of coins?
*/


package		rrs.euler


/**
	class Euler31
*/
object		Euler31
{
	val denominations = List(200, 100, 50, 20, 10, 5, 2, 1)


	var nDeductions = 0

	def
	countDown(total: Int, remaining: Int, denoms: List[Int]): Unit =
		if (remaining == 0)
			nDeductions += 1
		else {
			val smaller = denoms.dropWhile(_ > remaining)
			if (smaller != Nil) {
				val denom = smaller.head
				countDown(total, remaining - denom, smaller)
				if (smaller.tail != Nil)
					countDown(total, remaining, smaller.tail)
			}
		}


	def
	euler31(amount: Int): Int = {
		countDown(amount, amount, denominations)
		nDeductions
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler31: %d%n", euler31(200))
}
