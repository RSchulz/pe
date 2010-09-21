/*
	Euler15.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	Starting in the top left corner of a 2×2 grid, there are 6 routes
	(without backtracking) to the bottom right corner.

	How many routes are there through a 20×20 grid?
*/


package		rrs.euler


object		Euler15
{
//	def
//	binomRecur(n: Int, k: Int): Int =
//		if (n == 0 || k == 0 || k >= n)
//			1
//		else
//			binomRecur(n - 1, k - 1) + binomRecur(n - 1, k)

	def
	binomial(n: BigInt, k: BigInt): BigInt =
		fact(n) / (fact(k) * fact(n - k))

	def
	fact(n: BigInt): BigInt = {
		var f = BigInt(1)
		var nn = n
		while (nn > 1) {
			f = f * nn
			nn -= 1
		}
		f
	}

	def
	euler15(n: Int): BigInt =
		binomial(BigInt(n) * BigInt(2), BigInt(n))

	def
	main(args: Array[String]): Unit =
		printf("Euler15: %s%n", euler15(20))
}
