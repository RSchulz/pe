/*
	Euler48.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

	Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
*/

package		rrs.euler


/**
	class Euler48
*/
object		Euler48
{
	import math._

	val bi10e10 = BigInt(10000000000l)

	def
	euler48(limit: Int): BigInt =
	(
		(BigInt(0) /: (1 to 1000 map(BigInt(_)))) { (sum: BigInt, next: BigInt) => sum + next.modPow(next, bi10e10) }
		% bi10e10
	)

	def
	main(args: Array[String]): Unit =
		printf("Euler48: %s%n", euler48(1000))
}
