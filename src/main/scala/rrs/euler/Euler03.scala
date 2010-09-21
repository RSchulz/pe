/*
	Euler03.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The prime factors of 13195 are 5, 7, 13 and 29.

	What is the largest prime factor of the number 600851475143?
*/

package		rrs.euler


/**
	class Euler03
*/
object		Euler03
{
	def
	euler03(target: Long): Long = {
		val pf = (Stream.from(2) takeWhile(_ <= target) filter(target % _ == 0)).head
		if (pf == target)
			target
		else
			euler03(target / pf)
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler03: %d%n", euler03(600851475143l))
}
