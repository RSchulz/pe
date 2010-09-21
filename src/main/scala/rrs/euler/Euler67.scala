/*
	Euler67.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	By starting at the top of the triangle below and moving to adjacent numbers
	on the row below, the maximum total from top to bottom is 23.

			   3
			  7 4
			 2 4 6
			8 5 9 3

	That is, 3 + 7 + 4 + 9 = 23.

	Find the maximum total from top to bottom in
	<http://projecteuler.net/project/triangle.txt>,
	a 15K text file containing a triangle with one-hundred rows.

	NOTE: This is a much more difficult version of Problem 18.
	It is not possible to try every route to solve this problem, as there are 2^(99) altogether!
	If you could check one trillion (10^(12)) routes every second it would take over twenty billion
	years to check them all. There is an efficient algorithm to solve it. ;o)
*/

package		rrs.euler


/**
	object Euler67
*/
object		Euler67
{
	import io.Source
	import collection.mutable.ArraySeq
	import math.max

	type Triangle = Array[Array[Int]]
	type TriPath = ArraySeq[Int]

	def
	path(pathElements: Int*): TriPath =
		ArraySeq(pathElements: _*)


	def
	readTriangle(triFile: String): Triangle = {
		val triSplit = "\\s+"
		val triSource = Source.fromFile(triFile)
		val triLines = triSource.getLines.toSeq
		val triangle = Array((triLines.map { line => Array[Int]((line.split(triSplit).map(_.toInt)): _*) }): _*)
		triSource.close
		triangle
	}


	def
	rowMaxes(tangle: Triangle, below: Array[Int], d: Int): Array[Int] =
		(for (i <- 0 to d) yield (tangle(d)(i) + max(below(i), below(i + 1)))).toArray

	def
	triMax(tangle: Triangle): Int =
		triMax(tangle, 0)(0)

	def
	triMax(tangle: Triangle, d: Int): Array[Int] =
		if (d == tangle.length - 1)
			tangle(d)
		else
			rowMaxes(tangle, triMax(tangle, d + 1), d)

	def
	euler67(tangle: Triangle): Int =
		triMax(tangle)

	def
	main(args: Array[String]): Unit =
		args foreach { arg =>
			val argTangle = readTriangle(arg)
			printf("Euler67: \"%s\" => %s%n", arg, euler67(argTangle))
		}
}
