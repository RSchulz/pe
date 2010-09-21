/*
	Euler35.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	The number, 197, is called a circular prime because all rotations of the digits:
	197, 971, and 719, are themselves prime.

	There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

	How many circular primes are there below one million?
*/


package		rrs.euler


/**
	class Euler35
*/
object		Euler35
{
	import rrs.num._
	import PrimesL._


	/* All primes have a decimal presentation whose least-significant digit is one of: 1, 3, 7, 9 */
	val primeDigits		= Array(1, 3, 7, 9)

	val limit			= 1000000

	val nPLongs10e6		= 78498 // primeLongs(limit, pLongs10e6)
	val pLongs10e6		= new Array[Long](nPLongs10e6)
						  primeLongs(limit, pLongs10e6)
//	val pBits10e6		= primeBits(limit)

	def
	isPrime(n: Int): Boolean =
		if (n == 2)
			return true
		else {
			var i = 0
			val lastFactor = n / 2 + 1
			while (i < nPLongs10e6) {
				val p_i = pLongs10e6(i)
				if (p_i >= lastFactor)
					return true
				else
				if (n % p_i == 0)
					return false
				else
					i += 1
			}
			return true
		}


	def
	rotate(n: Int, r: Int, w: Int): Int =
		if (r == 0)
			n
		else
			rotate(rotate1(n, w), r - 1, w)

	def
	rotate1(n: Int, w: Int): Int =
		n / 10 + n % 10 * pow(10, (w - 1)).toInt

	def
	isCircularPrime(n: Int, w: Int): Boolean =
		isPrime(n) && (((1 until w) map (rotate(n, _, w))) forall (isPrime(_)))


	def
	zero4(nDigits: Int): Array[Int] =
		new Array[Int](nDigits)

	def
	inc4(n: Array[Int]): Unit = {
		val nDigits = n.length
		var d = 0
		while (d < nDigits) {
			val dv = n(d)
			if (dv == 3)
				n(d) = 0
			else {
				n(d) = dv + 1
				return
			}
			d += 1
		}
	}

	def
	val4(n: Array[Int]): Int = {
		val nDigits = n.length
		var v = 0
		var pow = 1
		var d = 0
		while (d < nDigits) {
			v += n(d) * pow
			pow *= 4
			d += 1
		}
		v
	}

	def
	val4D(n: Array[Int], radix: Int, digits: Array[Int]): Int = {
		val nDigits = n.length
		var v = 0
		var pow = 1
		var d = 0
		while (d < nDigits) {
			v += digits(n(d)) * pow
			pow *= radix
			d += 1
		}
		v
	}

	def
	nCP(w: Int): Int =
		if (w ==1)
			4
		else {
			val r4 = zero4(w)
			var n = 0
			val nVals = pow(4, w).toInt

			(0 until nVals) foreach { _ =>
//				val v4D = val4D(r4, 10, primeDigits)
//				if (isCircularPrime(v4D, w)) { n += 1; println(v4D) }; inc4(r4)
				if (isCircularPrime(val4D(r4, 10, primeDigits), w))  n += 1; inc4(r4)
			}

			n
		}


	def
	euler35: Int =
		nCP(1) + nCP(2) + nCP(3) + nCP(4) + nCP(5) + nCP(6)

	def
	main(args: Array[String]): Unit =
		printf("Euler35: %d%n", euler35)
}
