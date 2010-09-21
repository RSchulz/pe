/*
	Euler04.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	A palindromic number reads the same both ways.
	The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

	Find the largest palindrome made from the product of two 3-digit numbers.
*/

package		rrs.euler


/**
	class Euler04
*/
object		Euler04
{
	def
	isPalindromic(num: Long): Boolean = {
		val str = num.toString
		str == str.reverse
	}

	def
	euler04(min: Long, max: Long): Long = {
		var maxP: Long = 0
		for {
			i <- max to min by -1
			j <- max to min by -1
			p = i * j
			if isPalindromic(p)
		} {
			if (p > maxP)
				maxP = p
		}

		maxP
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler04: %d%n", euler04(100, 999))
}
