/*
	Euler39.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	If p is the perimeter of a right angle triangle with integral length sides,
	{a, b, c}, there are exactly three solutions for p = 120.

		{20,48,52}, {24,45,51}, {30,40,50}

	For which value of p ≤ 1000 is the number of solutions maximised?
*/


package		rrs.euler


/**
	class Euler39
*/
object		Euler39
{
	import rrs.num._


	/* Pythagorean triples */
	type PyTriple = (Int, Int, Int)

	def
	pPyTriples(nMax: Int): Stream[PyTriple] =
		for {
			n	<-	(2 to nMax).toStream
			n2	=	n * n
			m	<-	1 until n
			m2	=	m * m
		} yield
			(n2 - m2, 2 * n * m, n2 + m2)


	/* Pythagorean triples with added a + b + c component */
	type PyQuad = (Int, Int, Int, Int)

	/* Generate primitive Pythagorean quads for all values of N less than or equal to <i>nMax</i> */
	def
	pPyQuads(nMax: Int): Stream[PyQuad] =
		for {
			n	<-	(2 to nMax).toStream
			n2	=	n * n
			m	<-	1 until n
			m2	=	m * m
			if		((m % 2 + n % 2) == 1) && coprime(m, n)
		} yield {
			val a = n2 - m2
			val b = 2 * n * m
			val c = n2 + m2
			(a, b, c, a + b + c)
		}


	def
	euler39: (Int, Int) = {
		val quads = pPyQuads(1000) filter(_._4 <= 1000) toArray

		(for (p <- 3 to 1000) yield (p, quads count (p % _._4 == 0))) max Ordering.by((q: (Int, Int)) => q._2)
	}

	def
	main(args: Array[String]): Unit = {
		val answer = euler39
		printf("Euler39: p=%d; n=%d%n", answer._1, answer._2)
	}
}
