/*
	Euler53.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	There are exactly ten ways of selecting three from five, 12345:

		123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

	In combinatorics, we use the notation, ^(5)C_(3) = 10.

	In general:

			C(n, r) = n! / (r! * (n − r)!)

		where r ≤ n

	It is not until n = 23, that a value exceeds one-million: ^(23)C_(10) = 1144066.

	How many, not necessarily distinct, values of  C(n, r),
	for 1 ≤ n ≤ 100, are greater than one-million?
*/

package		rrs.euler


/**
	class Euler53
*/
object		Euler53
{
	def
	C(n: Int, r: Int): BigInt =
		fact(n) / fact(r) / fact(n - r)

	def
	fact(n: Int): BigInt = {
		var nn = n
		var f = BigInt(1)
		while (nn > 1) {
			f *= nn
			nn -= 1
		}
		f
	}

//	assert(fact(0) == 1)
//	assert(fact(5) == 120)


	def
	euler53: Int =
		( for {
			n	<-	1 to 100
			r	<-	1 to n
			nCr	=	C(n, r)
			if		nCr > 1000000
		  } yield	nCr
		) length

	def
	main(args: Array[String]): Unit =
		printf("Euler53: %s%n", euler53)
}
