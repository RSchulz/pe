/*
	Euler16.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	2^(15) = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

	What is the sum of the digits of the number 2^(1000)?
*/

package		rrs.euler


/**
	class Euler16
*/
object		Euler16
{
	def
	euler16(pow2: Int): Int = {
		var pow = BigInt(1)
		1 to pow2 foreach { _ => pow = pow * 2 }
		(0 /: pow.toString) ((s: Int, c: Char) => s + (c - '0'))
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler16: %d%n", euler16(1000))
}
