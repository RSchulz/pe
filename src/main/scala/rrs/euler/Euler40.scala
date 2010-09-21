/*
	Euler40.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	An irrational decimal fraction is created by concatenating the positive integers:

		0.123456789101112131415161718192021...
		             ^
		            12th

	It can be seen that the 12th digit of the fractional part is 1.

	If d_n represents the n-th digit of the fractional part, find the value of the following expression.

		d_1 × d_10 × d_100 × d_1000 × d_10000 × d_100000 × d_1000000
*/


package		rrs.euler


/**
	class Euler40
*/
object		Euler40
{
	/*
		decade		n digits	values							from		to
		1			1			1 to 9							1			9
		2			180			10 to 99						10			189
		3			2700		100 to 999						190			2889
		4			36000		1000 to 9999					2890		38889
		5			450000		10000 to 99999					38890		488889
		6			5400000		100000 to 9999999				488890		5888889
	*/

	def
	d(n: Int): Int = {
		require(n > 0 && n < 5888889)

		if (n < 10)
			n
		else
		if (n < 190) {
			val nn = 10 + (n - 10) / 2
			if (n % 2 != 0)
				nn % 10
			else
				nn / 10
		}
		else
		if (n < 2890) {
			val nnn = 100 + (n - 190) / 3
			((n - 190) % 3) match {
				case 0 =>  nnn / 100
				case 1 => (nnn / 10) % 10
				case 2 =>  nnn % 10
			}
		}
		else
		if (n < 38890) {
			val nnnn = 1000 + (n - 2890) / 4
			((n - 2890) % 4) match {
				case 0 =>  nnnn / 1000
				case 1 => (nnnn / 100) % 10
				case 2 => (nnnn / 10) % 10
				case 3 =>  nnnn % 10
			}
		}
		else
		if (n < 488890) {
			val nnnnn = 10000 + (n - 38890) / 5
			((n - 38890) % 4) match {
				case 0 =>  nnnnn / 10000
				case 1 => (nnnnn / 1000) % 10
				case 2 => (nnnnn / 100) % 10
				case 3 => (nnnnn / 10) % 10
				case 4 =>  nnnnn % 10
			}
		}
		else {
			val nnnnnn = 100000 + (n - 488890) / 6
			((n - 488890) % 5) match {
				case 0 =>  nnnnnn / 100000
				case 1 => (nnnnnn / 10000) % 10
				case 2 => (nnnnnn / 1000) % 10
				case 3 => (nnnnnn / 100) % 10
				case 4 => (nnnnnn / 10) % 10
				case 5 =>  nnnnnn % 10
			}
		}
	}

	def
	euler40: Int =
	(	d(1)
	 *	d(10)
	 *	d(100)
	 *	d(1000)
	 *	d(10000)
	 *	d(100000)
	 *	d(1000000) )

//	def
//	euler40: Int =
//		(for (n <- 0 to 5) yield d(pow(10, n).toInt)) product

	def
	main(args: Array[String]): Unit =
		printf("Euler40: %d%n", euler40)
}
