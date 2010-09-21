/*
	Euler52.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	It can be seen that the number, 125874, and its double, 251748,
	contain exactly the same digits, but in a different order.

	Find the smallest positive integer, x, such that
	2x, 3x, 4x, 5x, and 6x, contain the same digits.
*/

package		rrs.euler


/**
	class Euler52
*/
object		Euler52
{
	def
	ints(n: Int): Stream[Int] =
		n #:: ints(n + 1)

	def
	sameChars(str1: String, str2: String): Boolean =
		str1.length == str2.length && (str1 forall (str2.contains(_)))

	def
	euler52: Int = {
		( ints(1) dropWhile { n =>
			val nStr = n.toString
			!((2 to 6) forall { m => sameChars(nStr, (n * m).toString) })
		} ) head
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler52: %s%n", euler52)
}
