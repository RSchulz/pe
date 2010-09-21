/*
	Euler20.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	n! means n × (n − 1) × ... × 3 × 2 × 1

	Find the sum of the digits in the number 100!
*/

package		rrs.euler


/**
	class Euler20
*/
object		Euler20
{
	def
	euler20(n: Int): Int = {
		var fact = BigInt(1)
		1 to n foreach { i => fact = fact * i }
		(0 /: fact.toString) ((s: Int, c: Char) => s + (c - '0'))
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler20: %d%n", euler20(100))
}
