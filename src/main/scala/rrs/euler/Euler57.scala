/*
	Euler57.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	It is possible to show that the square root of two can
	be expressed as an infinite continued fraction.

		√ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

	By expanding this for the first four iterations, we get:

		1 + 1/2 = 3/2 = 1.5
		1 + 1/(2 + 1/2) = 7/5 = 1.4
		1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
		1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

	The next three expansions are 99/70, 239/169, and 577/408, but the
	eighth expansion, 1393/985, is the first example where the number of
	digits in the numerator exceeds the number of digits in the denominator.

	In the first one-thousand expansions, how many fractions contain
	a numerator with more digits than denominator?
*/


package		rrs.euler


/**
	class Euler57
*/
object		Euler57
{
	import rrs.num._

	/*
		Numerators:
		n:	0	1	2    3   4   5   6   7
			-	-	-	--	--	--	---	---
			1	3	7	17	41	99	239	577

		N_0		= 1
		N_1		= 3
		N_n+1	= 2 * N_n + N_n-1
	*/

	def
	numStream(n: BigInt, m: BigInt): Stream[BigInt] =
		n #:: numStream(m, m * 2 + n)

	def
	numerators: Stream[BigInt] =
		numStream(BigInt(1), BigInt(3)).drop(1)


	/*
		Denominators:
		n:	0	1	2    3   4   5   6   7
			-	-	-	--	--	--	---	---
			1	2	5	12	29	70	169	408

		N_0		= 1
		N_1		= 2
		N_n+1	= 2 * N_n + N_n-1
	*/

	def
	denStream(n: BigInt, m: BigInt): Stream[BigInt] =
		n #:: numStream(m, m * 2 + n)

	def
	denominators: Stream[BigInt] =
		denStream(BigInt(1), BigInt(2)).drop(1)


	def
	euler57: BigInt =
		(numerators zip denominators) take(1000) filter(fract => nDigits(fract._1) > nDigits(fract._2)) length

	def
	main(args: Array[String]): Unit =
		printf("Euler57: %s%n", euler57)
}
