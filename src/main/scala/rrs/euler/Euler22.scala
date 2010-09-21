/*
	Euler22.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	Using names.txt (<http://projecteuler.net/project/names.txt>),
	a 46K text file containing over five-thousand first names,
	begin by sorting it into alphabetical order.
	Then working out the alphabetical value for each name,
	multiply this value by its alphabetical position in the list to obtain a name score.

	For example, when the list is sorted into alphabetical order,
	COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name
	in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

	What is the total of all the name scores in the file?
*/

package		rrs.euler


/**
	class Euler22
*/
object		Euler22
{
	import io.Source
	import collection.mutable.ArrayBuffer


	def
	euler22(source: Source): Long = {
		val names = new ArrayBuffer[String]
		while (source.hasNext) {
			val qName = source takeWhile(_ != ',') mkString("")
			names += qName.substring(1, qName.length - 1)
		}
		( names.sorted.zipWithIndex map { ni => (ni._1 map (_ - 'A' + 1) sum) * (ni._2 + 1) } ) sum
	}

	def
	main(args: Array[String]): Unit =
		args foreach { arg =>
			val argSource = Source.fromFile(arg)
			printf("Euler22: \"%s\" => %d%n", arg, euler22(argSource))
		}
}
