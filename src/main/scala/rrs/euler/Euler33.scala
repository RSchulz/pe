/*
	Euler33.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The fraction 49/98 is a curious fraction, as an inexperienced mathematician
	in attempting to simplify it may incorrectly believe that 49/98 = 4/8,
	which is correct, is obtained by cancelling the 9s.

	We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

	There are exactly four non-trivial examples of this type of fraction,
	less than one in value, and containing two digits in the numerator and denominator.

	If the product of these four fractions is given in its lowest common terms,
	find the value of the denominator.
*/


package		rrs.euler


/**
	class Euler33
*/
object		Euler33
{
	import rrs.num._


	def
	isCurious(num: Int, denom: Int): Boolean = {
		val nDigits = num.toString.toList
		val dDigits = denom.toString.toList - '0'
		val ndDigits = nDigits intersect dDigits - '0'

		if (ndDigits.isEmpty)
			false

		else {
			val cd = gcd(num, denom)
			val rat = (num / cd, denom / cd)

			( for (ndDigit <- ndDigits) yield {
				val rnDigits = nDigits - ndDigit
				val rdDigits = dDigits - ndDigit

				( (0 /: rnDigits)(_ * 10 + _ - '0'),
				  (0 /: rdDigits)(_ * 10 + _ - '0') )
			  }
			) exists { frac =>
				reduceI(frac) == rat
			}
		}
	}

	def
	euler33: Int =
		reduceI(((1, 1) /: ( for {
								denom	<- 11 until 100
								num		<-  1 until denom
								if 		isCurious(num, denom)
							  } yield	(num, denom)
							)
				) ( (prod: (Int, Int), rat: (Int, Int)) => (prod._1 * rat._1, prod._2 * rat._2) )
		)._2

	def
	main(args: Array[String]): Unit =
		printf("Euler33: %d%n", euler33)
}
