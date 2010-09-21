/*
	Stats.scala
		Statistical methods

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/


package		rrs.num


/**

*/

object		Stats
{
	import math._


	/** Compute the discrete mean and standard deviation of a collection of Numeric values */
	def
	msd[N : Numeric](values: Traversable[N]): (Int, Double, Double) = {
		val nops = implicitly[Numeric[N]]
		import nops._

		var n = 1
		val sum = values.reduceLeft{ (s: N, v: N) => n += 1; s + v }
		val mean = sum.toDouble / n
		val variance = (0.0d /: values) { (x: Double, y: N) =>
											val diff = (y.toDouble - mean)
											x + diff * diff }
		(n, mean, math.sqrt(variance / n))
	}
}
