/*
	Euler79.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	A common security method used for online banking is to
	ask the user for three random characters from a passcode.
	For example, if the passcode was 531278, they may ask for
	the 2nd, 3rd, and 5th characters; the expected reply would be: 317.

	The text file, keylog.txt, contains fifty successful login attempts.

	Given that the three characters are always asked for in order, analyse the
	file so as to determine the shortest possible secret passcode of unknown length.
*/

package		rrs.euler



/**
	object Euler79
*/
object		Euler79
{
	import io.Source
	import util.matching.Regex
	import collection.mutable.ArrayBuffer

	type Pass = IndexedSeq[Int]
	type Keys = IndexedSeq[Int]


	def
	readKeys(keyFile: String): Keys = {
		val keySource = Source.fromFile(keyFile)
		val keyBuffer = new ArrayBuffer[Int]
		keySource.getLines.foreach { line => keyBuffer += line.toInt }
		keySource.close
		keyBuffer
	}

	def
	digitsUsed(keys: Keys): Seq[Int] =
		(keys flatMap(_.toString.toList map(_.toInt - '0')) distinct).sorted

	def
	keyRE(key: Int, digitsUsed: String): Regex =
		"[%s]*%s$".format(digitsUsed, (key.toString.flatMap(c => "%c[%s]*".format(c, digitsUsed)))).r

	def
	matchesAll(s: String, regexes: Seq[Regex]): Boolean =
		regexes forall { re => re.findPrefixOf(s).isDefined }


	def
	euler79(keys: Keys): Int = {
		val digits = digitsUsed(keys).mkString("")
		val keyREs = keys map(keyRE(_, digits))

//		(ints(100) filter(i => matchesAll(i.toString, keyREs)) head).toInt

		var i = 0
		while(i < Int.MaxValue) {
			if (matchesAll(i.toString, keyREs))
				return i
			i += 1
		}
		return 0
	}

	def
	main(args: Array[String]): Unit =
		args foreach { arg =>
			val argKeys = readKeys(arg)
			printf("Euler79: \"%s\" => %d%n", arg, euler79(argKeys))
		}
}
