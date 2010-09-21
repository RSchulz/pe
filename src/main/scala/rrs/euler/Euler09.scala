/*
	Euler09.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	A Pythagorean triplet is a set of three natural numbers, a < b < c,
	for which a^2 + b^2 = c^2

	For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

	There exists exactly one Pythagorean triplet for which a + b + c = 1000.
	Find the product abc.
*/

package		rrs.euler


/**
	class Euler09
*/
object		Euler09
{
	def
	euler09: Long =
	(	for {
			a	<- 1 to 1000
			b	<- a to (1000 - a)
			c	<- b to (1000 - b)
			if	a * a + b * b == c * c
			if	a + b + c == 1000
		}
		yield
			a * b * c
	)	head

	def
	main(args: Array[String]): Unit =
		printf("Euler09: %d%n", euler09)
}
