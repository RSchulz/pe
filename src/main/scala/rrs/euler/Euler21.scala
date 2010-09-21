/*
	Euler21.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	Let d(n) be defined as the sum of proper divisors of n
	(numbers less than n which divide evenly into n).
	If d(a) = b and d(b) = a, where a ≠ b, then a and b are an
	amicable pair and each of a and b are called amicable numbers.

	For example, the proper divisors of 220 are
	1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
	therefore d(220) = 284.

	The proper divisors of 284 are 1, 2, 4, 71 and 142;
	so d(284) = 220.

	Evaluate the sum of all the amicable numbers under 10000.
*/

package		rrs.euler


/**
	class Euler21
*/
object		Euler21
{
	import rrs.num.Factorize._

	def
	d(n: Long): Long =
		n match {
			case 0 => 0
			case 1 => 0
			case _ => sumDivisors(n) - n
		}

	def
	areAmicable(m: Int, n: Int): Boolean =
		d(m) == n && d(n) == m

	def
	euler21(limit: Int): Long = {
		val dVals = ((0 until limit) map (d(_))).toArray
		( for { i		<-	1 until limit
				d_i		=	dVals(i)
					if		d_i > 0 && d_i != i && d_i < limit
				d_i_i	=	dVals(d_i.toInt)
					if		d_i_i == i
			} yield i
		) sum
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler21: %d%n", euler21(10000))
}
