/*
	Euler01.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	If we list all the natural numbers below 10 that are multiples of 3 or 5,
	we get 3, 5, 6 and 9. The sum of these multiples is 23.

	Find the sum of all the multiples of 3 or 5 below 1000.
*/

package		rrs.euler


/**
	class Euler01
*/
object		Euler01
{
	def
	euler01(limit: Int): Long =
		(0l /: (
				for {	i	<- 3 until limit
						if	i % 3 == 0 || i % 5 == 0
				}
				yield i
			)
		) (_ + _)

	def
	main(args: Array[String]): Unit =
		printf("Euler01: %d%n", euler01(1000))
}
