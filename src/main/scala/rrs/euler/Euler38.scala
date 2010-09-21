/*
	Euler38.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	Take the number 192 and multiply it by each of 1, 2, and 3:

		192 × 1 = 192
		192 × 2 = 384
		192 × 3 = 576

	By concatenating each product we get the 1 to 9 pandigital, 192384576.
	We will call 192384576 the concatenated product of 192 and (1,2,3)

	The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5,
	giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

	What is the largest 1 to 9 pandigital 9-digit number that can be formed
	as the concatenated product of an integer with (1,2, ... , n) where n > 1?
*/


package		rrs.euler


/**
	class Euler38
*/
object		Euler38
{
	import rrs.num._


	def
	concatProd(i: Long, n: Int): Long =
		(0l /: (1 to n)) { (cp: Long, m: Int) =>
			val p = i * m
			cp * mag(p) * 10 + p
		}

	val nDigits = Array(0,
						0x0001, 0x0003, 0x0007, 0x000f, 0x001f,
						0x003f, 0x007f, 0x00ff, 0x01ff, 0x03ff)

	def
	isPandigital(i: Long, n: Int): Boolean = {
		val ns = i.toString
		(ns.length == n &&
		 ((0 /: ns) { (dBits: Int, d: Char) => if ('d' != '0') dBits | 1 << (d - '1') else dBits }) == nDigits(n))
	}


	def
	euler38: (Int, Int, Long) =
		( for {
			m	<-	1 to 100000
			n	<-	2 to 12
			cp	=	concatProd(m, n)
			if		isPandigital(cp, 9)
		} yield (m, n, cp) ) max(Ordering.by((cp: (Int, Int, Long)) => cp._3))

	def
	main(args: Array[String]): Unit = {
		val answer = euler38
		printf("Euler38: m=%d; n=%d; cp=%d%n", answer._1, answer._2, answer._3)
	}
}
