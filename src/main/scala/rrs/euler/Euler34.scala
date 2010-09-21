/*
	Euler34.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

	Find the sum of all numbers which are equal to the sum of the factorial of their digits.

	Note: as 1! = 1 and 2! = 2 are not sums they are not included.
*/


package		rrs.euler


/**
	class Euler34
*/
object		Euler34
{
	import rrs.num.lFact

	def
	isCurious(n: Int): Boolean =
		n.toString.map(d =>lFact(d - '0')).sum == n


	def
	euler34: Int =
		((4 to 10000000) filter (isCurious(_))).sum

	def
	main(args: Array[String]): Unit =
		printf("Euler34: %d%n", euler34)
}
