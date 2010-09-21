/*
	Primes.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

package		rrs.num


import rrs.util.{TBVI, TBVL, TBVBI}

import scala.collection.mutable.BitSet


object		Primality
{
	import rrs.num._

	/**
		Miller-Rabin primality test adapted from
			http://en.literateprograms.org/Miller-Rabin_primality_test_%28Scala%29
	*/

	private
	def
	millerRabin1(a: Long, n: Long): Boolean = {
		var d = n - 1
		var s = 0l

		while (d % 2 == 0) {
			d >>= 1
			s += 1
		}

		var aPow = modPow(a, d, n)

		if (aPow == 1)
			return true

		var j = s.intValue
		while (j > 0) {
			if (aPow == n - 1)
				return true
			else
				aPow = modPow(aPow, 2, n) // (aPow * aPow) % n

			j -= 1
		}

		aPow == n - 1
	}

	def
	millerRabin(n: Long, k: Int): Boolean = {
		var kk: Int = k
		while (kk > 0) {
			var a = 0l
			while (a == 0)
				a = ((rand.nextDouble * (n - 1))).toLong
			if (!millerRabin1(a, n))
				return false

			kk -= 1
		}
		true
	}

	private val rand: scala.util.Random = new scala.util.Random

	def
	isPrime(n: Long): Boolean =
		n > 1 && (n == 2 || (n % 2 == 1 && millerRabin(n, 25)))


//	/** Compute a^b mod m for Longs */
//	def
//	modPow(a: Long, b: Long, m: Long): Long = {
//		val result = rrs.num.modPow(a, b, m)
//		val biResult = BigInt(a).modPow(b, m)
//		if (result != biResult.toLong)
//			printf("modPow(%d, %d, %d): %dl != BigInt(%s)%n", a, b, m, result, biResult)
//		biResult.toLong
//	}


	private
	def
	millerRabin1(a: BigInt, n: BigInt): Boolean = {
		var d: BigInt = 0
		var s: BigInt = 0

		d = n - 1
		s = 0
		while (d % 2 == 0) {
			d >>= 1
			s += 1
		}

		var aPow = a.modPow(d, n)

		if (aPow == 1)
			return true

		var j = s.intValue
		while (j > 0) {
			if (aPow == n - 1)
				return true
			else
				aPow = (aPow * aPow) % n

			j -= 1
		}

		aPow == n - 1
	}

	def
	millerRabin(n: BigInt, k: Int): Boolean = {
		var kk: Int = k
		while (kk > 0) {
			var a: BigInt = 0
			val rand: scala.util.Random = new scala.util.Random()

			while (a == 0)
				a = BigInt((rand.nextDouble * n.doubleValue).toLong)

			if (!millerRabin1(a, n))
				return false

			kk -= 1
		}
		true
	}

	def
	isPrime(n: BigInt): Boolean =
		n > 1 && (n == 2 || (n.testBit(0) && millerRabin(n, 20)))
}


/**
	class PrimesL: Miscellaneous Prime Number Generation
*/
object		PrimesL
{
	def
	ints(n: Long): Stream[Long] =
		n #:: ints(n + 1)

	private
	def
	primeGen(nums: Stream[Long]): Stream[Long] =
		nums.head #:: (primeGen((nums tail) filter (x => x % nums.head != 0)))

	def
	primeStream: Stream[Long] =
		primeGen(ints(2))

	def
	primetorial(n: Int): Long =
		primeStream take n reduceLeft(_ * _)


	def
	primeBits(limit: Int): BitSet = {
		val primes = new BitSet(limit + 1)
		val compos = new TBVI(limit + 1)
		var newPrime = 2
		while (newPrime <= limit) {
			primes += newPrime
			var cancel = newPrime
			while (cancel <= limit) {
				compos(cancel) = true
				cancel += newPrime
			}
			while (newPrime <= limit && compos(newPrime))
				newPrime += 1
		}
		primes
	}

	def
	primeTBVI(limit: Int): TBVI = {
		val compos = new TBVI(limit + 1)
		var newPrime = 2
		compos(0) = true
		compos(1) = true
		while (newPrime <= limit) {
			var cancel = newPrime + newPrime
			while (cancel <= limit) {
				compos(cancel) = true
				cancel += newPrime
			}
			newPrime += 1
			while (newPrime <= limit && compos(newPrime))
				newPrime += 1
		}
		compos.inverted = true
		compos
	}

	def
	primeTBVL(limit: Long): TBVL = {
		val compos = new TBVL(limit + 1)
		var newPrime = 2l
		compos(0) = true
		compos(1) = true
		while (newPrime <= limit) {
			var cancel = newPrime + newPrime
			while (cancel <= limit) {
				compos(cancel) = true
				cancel += newPrime
			}
			newPrime += 1
			while (newPrime <= limit && compos(newPrime))
				newPrime += 1
		}
		compos.inverted = true
		compos
	}


	def
	primeInts(limit: Int, primes: Array[Int]): Int = {
		var nPrimes = 0
		var seek = 2
		val compos = new TBVI(limit)

		while (seek < limit) {
			primes(nPrimes) = seek
			nPrimes += 1
			var cancel = seek
			while (cancel < limit && cancel > 0) {
				compos(cancel) = true
				cancel += seek
			}
			while (seek < limit && seek > 0 && compos(seek))
				seek += 1
		}

		nPrimes
	}

	def
	primeLongs(limit: Long, primes: Array[Long]): Int = {
		var nPrimes = 0
		var seek = 2l
		val compos = new TBVL(limit)

		while (seek < limit) {
			primes(nPrimes) = seek
			nPrimes += 1
			var cancel = seek
			while (cancel < limit && cancel > 0) {
				compos(cancel) = true
				cancel += seek
			}
			while (seek < limit && seek > 0 && compos(seek))
				seek += 1
		}

		nPrimes
	}

	def
	primeBigInts(limit: BigInt, primes: Array[BigInt]): Int = {
		var nPrimes = 0
		var seek = BigInt(2)
		val compos = new TBVBI(limit)

		while (seek < limit) {
			primes(nPrimes) = seek
			nPrimes += 1
			var cancel = seek
			while (cancel < limit && cancel > 0) {
				compos(cancel) = true
				cancel += seek
			}
			while (seek < limit && seek > 0 && compos(seek))
				seek += 1
		}

		nPrimes
	}

	def
	pi(limit: Long): Long =
		(limit / math.log(limit)).toLong


	def
	phi(n: Long): Long = {
		val nf = Factorization(n).factors.map(pf => (pf.p - 1, pf.p))
		val pr = ((n, 1l) /: nf) ((acc: (Long, Long), rat: (Long, Long)) => (acc._1 * rat._1, acc._2 * rat._2 ))
		pr._1 / pr._2
	}


	def
	main(args: Array[String]): Unit = {
		val Num = "([0-9]+)".r
		val Adj = "--adjust=([0-9]+\\.[0-9]+)".r

		var print = false
		var piAdjust = 1.06d

		import System.err.{printf => ePrintf}

		args foreach {
			case "-p" => print = true

			case Adj(a) => piAdjust = a.toDouble

			case Num(n) =>
				val limit = n.toLong

				if (limit > Int.MaxValue) {
					val piEst = (pi(limit) * piAdjust).toLong
					val primes = new Array[Long](piEst.toInt)
					val nPrimes = primeLongs(limit, primes)

					ePrintf("# pi(%d)=%d (est %d / %d); p_%d=%d%n",
							limit.asInstanceOf[AnyRef],
							nPrimes.asInstanceOf[AnyRef],
							piEst.asInstanceOf[AnyRef],
							pi(limit).asInstanceOf[AnyRef],
							nPrimes.asInstanceOf[AnyRef],
							primes(nPrimes - 1).asInstanceOf[AnyRef])

					if (print)
						for (i <- 0 until nPrimes)
							println(primes(i))
				}
				else {
					val piEst = (pi(limit) * piAdjust).toInt
					val primes = new Array[Int](piEst)
					val nPrimes = primeInts(limit.toInt, primes)

					ePrintf("# pi(%d)=%d (est %d / %d); p_%d=%d%n",
							limit.asInstanceOf[AnyRef],
							nPrimes.asInstanceOf[AnyRef],
							piEst.asInstanceOf[AnyRef],
							pi(limit).asInstanceOf[AnyRef],
							nPrimes.asInstanceOf[AnyRef],
							primes(nPrimes - 1).asInstanceOf[AnyRef])

					if (print)
						for (i <- 0 until nPrimes)
							println(primes(i))
				}

			case _ =>
				ePrintf("Primes: Usage: Primes [ -p ] limit [...]%n")
		}
	}
}


//class		PrimeGen(initialLimit: Int)
//{
//	import	collection.mutable.{ArrayBuffer, BitSet}
//
//	private var limit = initialLimit
//			val primes = new ArrayBuffer[Int]
//	private val compos = new BitSet(initialLimit)
//
//
//	morePrimes(2, initialLimit)
//
//	private
//	def
//	morePrimes(from: Int, to: Int): Unit = {
//		var seek = from
//		while (seek < to) {
//			primes += seek
//			var cancel = seek
//			while (cancel < to) {
//				compos += cancel
//				cancel += seek
//			}
//			while (seek < to && compos(seek))
//				seek += 1
//		}
//	}
//
//	def
//	extend(newLimit: Int): Unit = {
//		morePrimes(limit, newLimit)
//		limit = newLimit
//	}
//}


/**
	class PrimesBI: BigInt Primes
*/
object		PrimesBI
{
	def
	primeGen(prime: BigInt): Stream[BigInt] =
		Stream.cons(prime,
				    primeGen(new BigInt(prime.bigInteger.nextProbablePrime)))

	def
	primeStream: Stream[BigInt] =
		primeGen(2)

	def
	primetorial(n: Int): BigInt =
		primeStream take n reduceLeft(_ * _)
}
