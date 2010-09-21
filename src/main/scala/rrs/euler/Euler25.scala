/*
	Euler25.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The Fibonacci sequence is defined by the recurrence relation:

		F_(n) = F_(n−1) + F_(n−2), where F_(1) = 1 and F_(2) = 1.

	Hence the first 12 terms will be:

		F_(1) = 1
		F_(2) = 1
		F_(3) = 2
		F_(4) = 3
		F_(5) = 5
		F_(6) = 8
		F_(7) = 13
		F_(8) = 21
		F_(9) = 34
		F_(10) = 55
		F_(11) = 89
		F_(12) = 144

	The 12th term, F_(12), is the first term to contain three digits.

	What is the first term in the Fibonacci sequence to contain 1000 digits?
*/

package		rrs.euler


/**
	class Euler25
*/
object		Euler25
{
	import rrs.num._
	import FibonacciBI.fibs
	import math._

	val log2 = log(2)
	val log10 = log(10)
	val bitsPerDigit = log10 / log2

	def
	bits(d: Int): Int =
		ceil(bitsPerDigit * d).toInt

	def
	euler25(d: Int): (BigInt, Int) = {
		val dBits = bits(d - 1)
		fibs.zipWithIndex.dropWhile(_._1.bitLength < dBits) filter(_._1.toString.length >= d) head
	}

	def
	main(args: Array[String]): Unit = {
		val answer = euler25(1000)
		printf("Euler25: Fib(%d):%n%s%n", answer._2 + 1, answer._1)
	}
}
