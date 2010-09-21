/*
	Euler36.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The decimal number, 585 = 1001001001_(2) (binary), is palindromic in both bases.

	Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

	(Please note that the palindromic number, in either base, may not include leading zeros.)
*/


package		rrs.euler


/**
	class Euler36
*/
object		Euler36
{
	def
	isPalindrome(s: String): Boolean =
		s == s.reverse

	def
	isDecimalPalindrome(i: Int): Boolean =
		isPalindrome(i.toString)

	def
	isBinaryPalindrome(i: Int): Boolean =
		isPalindrome(i.toBinaryString)


	def
	euler36(limit: Int): Long =
		(1 until limit) filter (i => isDecimalPalindrome(i) && isBinaryPalindrome(i)) sum

	def
	main(args: Array[String]): Unit =
		printf("Euler36: %d%n", euler36(1000000))
}
