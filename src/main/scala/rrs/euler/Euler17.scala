/*
	Euler17.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	If the numbers 1 to 5 are written out in words: one, two, three, four, five,
	then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

	If all the numbers from 1 to 1000 (one thousand) inclusive were
	written out in words, how many letters would be used?

	NOTE: Do not count spaces or hyphens.
	For example, 342 (three hundred and forty-two) contains 23 letters
	and 115 (one hundred and fifteen) contains 20 letters.
	The use of "and" when writing out numbers is in compliance with British usage.
*/


package		rrs.euler


/**
	class Euler17
*/
object		Euler17
{
	val fingers = Array("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
	val toes	= Array("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
						"sixteen", "seventeen", "eighteen", "nineteen", "twenty")
	val tens	= Array("X", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")

	def
	textify(n: Int): String = {
		val to = new StringBuilder
		textify(n, to)
		to.toString
	}

	def
	textify(n: Int, to: StringBuilder): Unit = {
		if (n >= 1000) {
			val fullThousands = n / 1000
			val remainder = n % 1000
			to append fingers(fullThousands)
			to append " thousand"
			if (remainder > 0) {
				to append ", "
				textify(remainder, to)
			}
		}
		else
		if (n >= 100) {
			val fullHundreds = n / 100
			val remainder = n % 100
			to append fingers(fullHundreds)
			to append " hundred"
			if (remainder > 0) {
				to append " and "
				textify(remainder, to)
			}
		}
		else
		if (n > 20) {
			val fullTens = n / 10
			val remainder = n % 10
			to append tens(fullTens)
			if (remainder > 0) {
				to append "-"
				textify(remainder, to)
			}
		}
		else
		if (n > 10) {
			val fullTens = n / 10
			val remainder = n % 10
			to append toes(n - 10)
		}
		else
			to append fingers(n)
	}

	def
	euler17: Int =
		(1 to 1000) map(textify(_) filter( c => c != ' ' && c != '-' && c != ',') length ) sum

	def
	main(args: Array[String]): Unit =
		printf("Euler17: %d%n", euler17)
}
