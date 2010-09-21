/*
	Euler63.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The 5-digit number, 16807=7^(5), is also a fifth power.
	Similarly, the 9-digit number, 134217728=8^(9), is a ninth power.

	How many n-digit positive integers exist which are also an nth power?
*/


package		rrs.euler


/**
	class Euler63
*/
object		Euler63
{
	import rrs.num._


	def
	ndnp(n: Int): Seq[(Int, Int, BigInt)] =
		for (d <- BigInt(1) to BigInt(10); p = d.pow(n); if nDigits(p) == n) yield (d.toInt, n, p)

	def
	nNDNP(n: Int): Int =
		ndnp(n).length


	def
	euler63(max: Int): Int =
		(for (n <- 1 to max) yield nNDNP(n)) sum

	def
	main(args: Array[String]): Unit =
		printf("Euler63: %s%n", euler63(22))
}
