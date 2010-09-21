/*
	Euler07.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
	we can see that the 6^(th) prime is 13.

	What is the 10001-st prime number?
*/

package		rrs.euler


/**
	class Euler07
*/
object		Euler07
{
	import rrs.num.PrimesBI._

	def
	euler07: BigInt =
		primeStream drop 10000 head

	def
	main(args: Array[String]): Unit =
		printf("Euler07: %d%n", euler07)
}
