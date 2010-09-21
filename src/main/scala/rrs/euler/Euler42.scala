/*
	Euler42.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The n^(th) term of the sequence of triangle numbers is given by,
	t_(n) = 1/2(n(n+1)) == (n^2 + n) / 2; so the first ten triangle numbers are:

		1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

	By converting each letter in a word to a number corresponding to its
	alphabetical position and adding these values we form a word value.
	For example, the word value for SKY is 19 + 11 + 25 = 55 = t_(10).
	If the word value is a triangle number then we shall call the word a triangle word.

	Using <http://projecteuler.net/project/words.txt>, a 16K text file containing
	nearly two-thousand common English words, how many are triangle words?
*/


package		rrs.euler


/**
	class Euler42
*/
object		Euler42
{
	import io.Source
	import collection.mutable.ArrayBuffer


	def
	triangle(n: Int): Int =
		(n * n + n) / 2

	def
	triangles: Stream[Int] =
		triStream(1)

	def
	triStream(n: Int): Stream[Int] =
		triangle(n) #:: triStream(n + 1)


	def
	readWords(wordFile: String): (Seq[String], Int) = {
		val wordSource = Source.fromFile(wordFile)
		val words = new ArrayBuffer[String]
		while (wordSource.hasNext) {
			val qName = wordSource takeWhile(_ != ',') mkString("")
			words += qName.substring(1, qName.length - 1)
		}
		wordSource.close
		(words, words map (_.length) max)
	}

	def
	wordVal(word: String): Int =
		(word map (_ - 'A' + 1)) sum

	def
	euler42(wordFile: String): Int = {
		val (words, longest) = readWords(wordFile)
		val maxWord = 26 * longest
		val triVals = Set((triangles takeWhile (_ <= maxWord)): _*)

		words count { word => triVals contains wordVal(word) }
	}

	def
	main(args: Array[String]): Unit =
		args foreach { arg => printf("Euler42: \"%s\" => %d%n", arg, euler42(arg)) }
}
