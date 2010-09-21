/*
	Euler58.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	Starting with 1 and spiralling anticlockwise in the following way,
	a square spiral with side length 7 is formed.

		37 36 35 34 33 32 31
		38 17 16 15 14 13 30
		39 18  5  4  3 12 29
		40 19  6  1  2 11 28
		41 20  7  8  9 10 27
		42 21 22 23 24 25 26
		43 44 45 46 47 48 49

	It is interesting to note that the odd squares lie along the bottom right
	diagonal, but what is more interesting is that 8 out of the 13 numbers
	lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

	If one complete new layer is wrapped around the spiral above, a square spiral
	with side length 9 will be formed. If this process is continued, what is the
	side length of the square spiral for which the ratio of primes along both
	diagonals first falls below 10%?
*/


package		rrs.euler


/**
	class Euler58
*/
object		Euler58
{
	import rrs.num._
	import Primality.millerRabin

	/*
	   1                                               0
		  101 100  99  98  97  96  95  94  93  92  91
		  102  65  64  63  62  61  60  59  58  57  90
		  103  66  37  36  35  34  33  32  31  56  89
		  104  67  38  17  16  15  14  13  30  55  88
		  105  68  39  18   5   4   3  12  29  54  87
		  106  69  40  19   6   1   2  11  28  53  86
		  107  70  41  20   7   8   9  10  27  52  85
		  108  71  42  21  22  23  24  25  26  51  84
		  109  72  43  44  45  46  47  48  49  50  83
		  110  73  74  75  76  77  78  79  80  81  82
		  111 112 113 114 115 116 117 118 119 120 121
	   2                                               3

				   0   1   2   3   4   5
			  __________________________
			  0 |  1   3  13  31  57  91
			  1 |  1   5  17  37  65 101
			  2 |  1   7  21  43  73 111
			  3 |  1   9  25  49  81 121

			 C(R) = R * 8
			   0  =  0
			   1  =  8
			   2  = 16
			   3  = 24
			   4  = 32
	*/


	val mrLimit = 25


	def
	isPrime(i: Long): Boolean =
		i >= 2 && millerRabin(i, mrLimit)


	def
	triangle8(n: Long): Long =
		(n * n + n) * 4

	def
	diag(radius: Long, phase: Long): Long = {
		require(radius >= 0)
		require(phase >= 0 && phase < 4)

		if (radius == 0)
			1
		else
			phase match {
				case 0 => 1 + triangle8(radius) - radius * 6
				case 1 => 1 + triangle8(radius) - radius * 4
				case 2 => 1 + triangle8(radius) - radius * 2
				case 3 => 1 + triangle8(radius)
			}
	}

	def
	nCornerPrimes(r: Long): Long =
		if (r == 0)
			0
		else {
			val phase3 = triangle8(r)

			nPT += 3

			(	(if (isPrime(1 + phase3 - r * 6)) 1 else 0)
			+	(if (isPrime(1 + phase3 - r * 4)) 1 else 0)
			+	(if (isPrime(1 + phase3 - r * 2)) 1 else 0) )
		//	+	(if (isPrime(1 + phase3))         1 else 0) // Phase 3: All odd squares; none prime
		}

	var nPT = 0


	def
	diameter(r: Long): Long =
		r * 2 + 1

	def
	side(r: Long): Long =
		r * 2 + 1

	def
	diagonals(r: Long): Long =
		r * 4 + 1

	def
	circumference(r: Long): Long =
		r * 4


	def
	euler58: (Long, Long) =
		(longs(1)
		 .map(i => (i, nCornerPrimes(i)))
		 .scanLeft((0l, 0l)) { (acc: (Long, Long), ncp: (Long, Long)) => (ncp._1, acc._2 + ncp._2) }
		 .drop(1)
		 .dropWhile((ic: (Long, Long)) => ic._2 * 10 >= diagonals(ic._1))
		 .head)

	def
	main(args: Array[String]): Unit = {
		val answer = euler58
		printf("Euler58: r=%d; side=%d; total primes: %d (nPT=%d)%n",
			   answer._1, side(answer._1), answer._2, nPT)
	}
}
