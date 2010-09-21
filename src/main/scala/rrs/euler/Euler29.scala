/*
	Euler29.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	Consider all integer combinations of a^(b) for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:

		2^(2)=4, 2^(3)=8, 2^(4)=16, 2^(5)=32
		3^(2)=9, 3^(3)=27, 3^(4)=81, 3^(5)=243
		4^(2)=16, 4^(3)=64, 4^(4)=256, 4^(5)=1024
		5^(2)=25, 5^(3)=125, 5^(4)=625, 5^(5)=3125

	If they are then placed in numerical order, with any repeats removed,
	we get the following sequence of 15 distinct terms:

		4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125

	How many distinct terms are in the sequence generated by
	a^(b) for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
*/

package		rrs.euler


/**
	class Euler29
*/
object		Euler29
{
	import rrs.num.{Factorization, PrimeFactors, Factorize}
	import rrs.num.Factorize._

	def
	euler29(limit: Int): Long = {
		val aFactorizations = (2 to limit) map (n => Factorization(n.toLong))

//		Set( ( for {
//				a <- 2 to 100
//				b <- 2 to 100
//			} yield {
//				aFactorizations(a - 2).exp(b)
//			}
//		): _*) size

		( for {
				a <- 2 to 100
				b <- 2 to 100
			} yield {
				aFactorizations(a - 2).exp(b)
			}
		).distinct.size
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler29: %d%n", euler29(100))
}
