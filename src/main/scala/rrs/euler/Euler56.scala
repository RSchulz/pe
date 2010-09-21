/*
	Euler56.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	A googol 10^100 is a massive number: one followed by one-hundred zeros;
	100^100  is almost unimaginably large: one followed by two-hundred zeros.
	Despite their size, the sum of the digits in each number is only 1.

	Considering natural numbers of the form, a^b,
	where a, b < 100, what is the maximum digital sum?
*/

package		rrs.euler


/**
	class Euler56
*/
object		Euler56
{
	def
	euler56: Int = {
		( for (a <- 1 until 100) yield {
			var aPowB = BigInt(1)
			( for (b <- 1 until 100) yield {
				aPowB *= a
				digitalSum(aPowB)
			} ) max
		} ) max
	}

	def
	digitalSum(i: BigInt): Int =
		(0 /: i.toString) {  (s: Int, d: Char) => s + (d - '0') }

	def
	main(args: Array[String]): Unit =
		printf("Euler56: %s%n", euler56)
}
