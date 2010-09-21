/*
	Euler24.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	A permutation is an ordered arrangement of objects.
	For example, 3124 is one possible permutation of the digits
	1, 2, 3 and 4. If all of the permutations are listed numerically
	or alphabetically, we call it lexicographic order.
	The lexicographic permutations of 0, 1 and 2 are:

	012   021   102   120   201   210

	What is the millionth lexicographic permutation of the digits
	0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
*/

package		rrs.euler


/**
	class Euler24
*/
object		Euler24
{
	import rrs.num.Permute._

	def
	euler24: String = {
		val p10 = perm0(10)
		1 to 999999 foreach(_ => advancePerm(p10))
		p10.mkString("")
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler24: %s%n", euler24)
}
