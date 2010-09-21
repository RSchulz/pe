/*
	Euler43.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The number, 1406357289, is a 0 to 9 pandigital number because it is
	made up of each of the digits 0 to 9 in some order, but it also has
	a rather interesting sub-string divisibility property.

	Let d_1 be the 1st digit, d_2 be the 2nd digit, and so on.
	In this way, we note the following:

		d_2 * d_3 * d_4  = 406 is divisible by  2
		d_3 * d_4 * d_5  = 063 is divisible by  3
		d_4 * d_5 * d_6  = 635 is divisible by  5
		d_5 * d_6 * d_7  = 357 is divisible by  7
		d_6 * d_7 * d_8  = 572 is divisible by 11
		d_7 * d_8 * d_9  = 728 is divisible by 13
		d_8 * d_9 * d_10 = 289 is divisible by 17

	Find the sum of all 0 to 9 pandigital numbers with this property.
*/


package		rrs.euler


/**
	class Euler43
*/
object		Euler43
{
	import rrs.num.Permute._

	val products = Seq(1, 2, 3, 5, 7, 11, 13, 17)

	def
	isEuler43(digits: Array[Int]): Boolean =
		(digits
		 .sliding(3)
		 .zip(products.iterator))
		.forall ((dp: (Array[Int], Int)) => digitsToLong(dp._1) % dp._2 == 0 )

	def
	digitsToLong(perm: Array[Int]): Long =
		(0l /: perm)(_ * 10 + _)

	def
	euler43: Long = {
//		permutations(10) filter(isEuler43(_)) map(digitsToLong) sum

		val e43Perm = perm0(10)
		var euler43Sum = 0l

		while (advancePerm(e43Perm))
			if (isEuler43(e43Perm))
				euler43Sum += digitsToLong(e43Perm)

		euler43Sum
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler43: %d%n", euler43)
}
