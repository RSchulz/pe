/*
	Euler99.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	Comparing two numbers written in index form like 2^(11) and 3^(7) is not difficult,
	as any calculator would confirm that 2^(11) = 2048 < 3^(7) = 2187.

	However, confirming that 632382^(518061) > 519432^(525806) would be much more difficult,
	as both numbers contain over three million digits.

	Using base_exp.txt (<http://projecteuler.net/project/base_exp.txt>), a 22K text
	file containing one thousand lines with a base/exponent pair on each line,
	determine which line number has the greatest numerical value.

	NOTE: The first two lines in the file represent the numbers in the example given above.
*/

package		rrs.euler


/**
	object Euler99
*/
object		Euler99
{
	import math.log
	import io.Source
	import collection.mutable.ArrayBuffer


	type Pair = (Int, Int)
	type LNPair = (Int, Int, Int)
	type LNEPair = (Int, Int, Int, BigInt)


	def
	readPairs(pairFile: String): IndexedSeq[LNPair] = {
		val pairSource = Source.fromFile(pairFile)
		val pairBuffer = new ArrayBuffer[Pair]

		pairSource.getLines.foreach { line =>
										val digits = line.trim.split(",")
										pairBuffer += ((digits(0).toInt, digits(1).toInt))
									}
		pairSource.close
		pairBuffer.zipWithIndex.map(ip => (ip._2 + 1, ip._1._1, ip._1._2))
	}

	def
	euler99(pairs: Seq[LNPair]): LNPair = {
		val orderedPairs = pairs sorted(Ordering.by((lnp: LNPair) => log(lnp._2) * lnp._3 ))

//		orderedPairs foreach { lnp =>
//								val ln = lnp._1
//								val base = lnp._2
//								val exp = lnp._3
//								val logBase = log(base)
//								printf("line %4d: base=%6d; log(%6d)=%9f; exp=%7d; log(%6d) * %7d = %14.12g%n",
//									   ln, base, base, logBase, exp, base, exp, logBase * exp)
//							}

		orderedPairs last
	}

	def
	main(args: Array[String]): Unit =
		args foreach { arg =>
			val argPairs = readPairs(arg)
			val answer = euler99(argPairs)
			printf("Euler99: \"%s\" => line=%d; base=%d; exp=%d%n",
				   arg, answer._1, answer._2, answer._3)
		}
}
