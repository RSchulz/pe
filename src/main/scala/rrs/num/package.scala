/*
	package rrs.num:
		Assorted numerical primitives in Int, Long and BigInt forms

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/


package		rrs


/*
	gcd			Compute the Greatest Common Divisor
	coprime		Ascertain whether two numbers are coprime (a.k.a. relatively prime) (have GCD of 1)
	lcm			Compute the Least Common Multiple
*/


/**

*/

package
object		num
{
	import math._


	def
	ints(i: Int): Stream[Int] =
		i #:: ints(i + 1)

	def
	longs(l: Long): Stream[Long] =
		l #:: longs(l + 1)

	def
	bigInts(b: BigInt): Stream[BigInt] =
		b #:: bigInts(b + 1)


	def
	pow(i: Long, p: Int): Long = {
		var pp = p
		var ii = 1l
		while (pp > 0) {
			ii *= i
			pp -= 1
		}
		ii
	}


	/* Int */

	def
	gcd(i: Int, j: Int): Int = {
		val ia = abs(i)
		val ja = abs(j)

		if (ja == 0)
			ia
		else
			gcdI(ja, ia % ja)
	}

	private
	def
	gcdI(i: Int, j: Int): Int =
		if (j == 0)
			i
		else
			gcdI(j, i % j)

	def
	relPrime(i: Int, j: Int): Boolean =
		gcd(i, j) == 1

	def
	coprime(i: Int, j: Int): Boolean =
		gcd(i, j) == 1


	def
	lcm(i: Int, j: Int): Long = {
		val ijGCD = gcd(i, j)
		abs((i / ijGCD) * (j / ijGCD))
	}



	/* Long */

	def
	gcd(i: Long, j: Long): Long = {
		val ia = abs(i)
		val ja = abs(j)

		if (ja == 0)
			ia
		else
			gcdL(ja, ia % ja)
	}

	private
	def
	gcdL(i: Long, j: Long): Long = {
		if (j == 0l)
			i
		else
			gcdL(j, i % j)
	}

	def
	coprime(i: Long, j: Long): Boolean =
		gcd(i, j) == 1l

	def
	lcm(i: Long, j: Long): BigInt =
		(BigInt(i) * BigInt(j)).abs / gcd(i, j)



	/* BigInt */

	def
	gcd(i: BigInt, j: BigInt): BigInt = {
		val ia = i.abs
		val ja = j.abs

		if (ja == 0)
			ia
		else
			gcdBI(ja, ia % ja)
	}

	private
	def
	gcdBI(i: BigInt, j: BigInt): BigInt =
		if (j == 0)
			i
		else
			gcdBI(j, i % j)

	def
	coprime(i: BigInt, j: BigInt): Boolean =
		gcd(i, j) == 1


	def
	lcm(i: BigInt, j: BigInt): BigInt =
		(i * j).abs / gcd(i, j)



	/* Reduce fractions to proper form */

	def
	reduceI(num: Int, denom: Int): (Int, Int) = {
		val cd = gcd(num, denom)
		(num / cd, denom / cd)
	}

	def
	reduceI(rat: (Int, Int)): (Int, Int) = {
		val cd = gcd(rat._1, rat._2)
		if (cd != 0)
			(rat._1 / cd, rat._2 / cd)
		else
			rat
	}


	def
	reduceI(num: Long, denom: Long): (Long, Long) = {
		val cd = gcd(num, denom)
		(num / cd, denom / cd)
	}

	def
	reduceL(rat: (Long, Long)): (Long, Long) = {
		val cd = gcd(rat._1, rat._2)
		(rat._1 / cd, rat._2 / cd)
	}


	/* Digit counting */

	private val log2 = math.log(2.0d)

	def
	nDBigits(i: Int): Int =
		(math.log(i) / log2 + 1).toInt

	def
	nBDigits(i: Long): Int =
		(math.log(i) / log2 + 1).toInt


	def
	nDigits(i: Int): Int =
		if (i < 0)
			nDigits(-i)
		else {
			var nDigits = 1
			var ii = i
			while (ii >= 10) {
				nDigits += 1
				ii /= 10
			}
			nDigits
		}

	def
	nDigits(i: Long): Int =
		if (i < 0)
			nDigits(-i)
		else {
			var nDigits = 1
			var ii = i
			while (ii >= 10) {
				nDigits += 1
				ii /= 10
			}
			nDigits
		}

	def
	nDigits(d: Double): Int =
		if (d < 0.0d)
			nDigits(-d)
		else
			if (d == 0.0d)
				1
			else {
				val log10d = math.log10(d)
				if (log10d == math.floor(log10d))
					math.floor(log10d).toInt + 1
				else
					math.ceil(log10d).toInt
			}

	def
	nDigits(i: BigInt): Int = {
		var ii = i.abs
		var nd = 1
		val ten = BigInt(10)
		while (ii >= ten) {
			ii /= ten
			nd += 1
		}
		nd
	}


	private val log16 = math.log(16.0d)

	def
	nXDigits(i: Int): Int =
		if (i < 0)
			nDigits(-i)
		else {
			var nDigits = 1
			var ii = i
			while (ii >= 16) {
				nDigits += 1
				ii /= 16
			}
			nDigits
		}

	def
	nXDigits(i: Long): Int =
		if (i < 0)
			nDigits(-i)
		else {
			var nDigits = 1
			var ii = i
			while (ii >= 16) {
				nDigits += 1
				ii /= 16
			}
			nDigits
		}


	def
	mag(n: Int): Int = {
		var nn = math.abs(n)
		var l = 1
		while (nn >= 10) {
			nn /= 10
			l *= 10
		}
		l
	}

	def
	mag(n: Long): Long = {
		var nn = math.abs(n)
		var l = 1l
		while (nn >= 10) {
			nn /= 10
			l *= 10
		}
		l
	}


	/** Compute a^b mod m for Longs */
	def
	modPow(a: Long, b: Long, m: Long): Long = {
		var aa		= a % m
		var result	= 1l
		var k		= 0l
		while (true) {
			assert(k < 63, "long overflow in modPow")
			val bitK = 1l << k
			if (bitK > b)
				return result
			if ((b & bitK) != 0)
				result = ((result % m) * (aa % m)) % m
//				result = ((BigInt(result) * BigInt(aa)) % m).toLong		// Slow
//				result = (result * aa) % m								// Susceptible to overflow

//			aa = BigInt(aa).modPow(2, m).toLong							// Slow
//			aa = (aa * aa) % m											// Susceptible to overflow
			aa %= m
			aa *= aa
			aa %= m

			k += 1
		}
		error("Call EPFL! (!true)")
	}


	def
	lFact(n: Int): Long = {
		require(n >= 0 && n <= 20, "lFact(%d): Long factorial limited to n >= 0 && n <= 20".format(n))
		var f = 1l
		var nn = 1
		while (nn <= n) {
			f *= nn
			nn += 1
		}
		f
	}

	def
	bFact(n: Int): BigInt = {
		var f = BigInt(1)
		var nn = 1
		while (nn <= n) {
			f *= nn
			nn += 1
		}
		f
	}
}
