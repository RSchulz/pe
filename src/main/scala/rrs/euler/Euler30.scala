/*
	Euler30.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	Surprisingly there are only three numbers that can be
	written as the sum of fourth powers of their digits:

		1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
		8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
		9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

	As 1 = 1^(4) is not a sum it is not included.

	The sum of these numbers is 1634 + 8208 + 9474 = 19316.

	Find the sum of all the numbers that can be written
	as the sum of fifth powers of their digits.
*/


package		rrs.euler


/**
	class Euler30
*/
object		Euler30
{
	import rrs.num.pow

	def
	digits(n: Long): Seq[Long] =
		n.toString.toList map (_ - ('0': Long))

	def
	checkDPow(n: Long, p: Int): Boolean =
		n == ((digits(n) map (d => pow(d, p))) sum)

	def
	euler30(pow: Int): Long =
		(2 to 1000000) filter (checkDPow(_, pow)) sum

	def
	main(args: Array[String]): Unit =
		printf("Euler30: 4 => %d; 5 => %d%n", euler30(4), euler30(5))
}
