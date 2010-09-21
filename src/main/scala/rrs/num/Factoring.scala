/*
	Factoring.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

package		rrs.num


import		scala.collection.mutable.ArrayBuffer



final
case
class		PrimeFactors[P : Integral](p: P, n: Int)
extends		Ordered[PrimeFactors[P]]
{
	def
	this(factor: P) =
		this(factor, 1)

	val pOps = implicitly[Integral[P]]

	override
	def
	compare(other: PrimeFactors[P]): Int =
		pOps.compare(p, other.p)

	override
	def
	toString: String =
		if (n == 1)
			p.toString
		else
			"%s^%d".format(p, n)
}

object		PrimeFactors
{
	def
	apply[P : Integral](p: P): PrimeFactors[P] =
		new PrimeFactors(p, 1)

	implicit
	def
	num2PrimeFactors[P : Integral](p: P): PrimeFactors[P] =
		new PrimeFactors(p, 1)
}


case
class		Factorization[P : Integral] private (n: P, factors: PrimeFactors[P]*)
{
	val distinct		= factors map (_.p) distinct
	val factorCards		= Map( (factors map (pf => pf.p -> pf.n) ): _* )
	lazy val nFactors	= factorCards.values.sum

	def
	nDistinct: Int =
		factors.length

	def
	card(factor: P): Int =
		factorCards.getOrElse(factor, 0)

	def
	exp(pow: Int): Factorization[P] = {
		val pOps = implicitly[Integral[P]]
		import pOps._
		var iPow = pow
		var nPow = pOps.one
		while (iPow > 0) {
			nPow *= n
			iPow -= 1
		}
		Factorization[P](nPow, (factors map (pf => PrimeFactors(pf.p, pf.n * pow))): _*)
	}

	def
	isPrime: Boolean =
		distinct.size == 1 && factors(0).n == 1

	override
	def
	toString: String =
		"%s | %s".format(n, factors.mkString(", "))

	def
	toString(fmt: String): String =
		fmt.format(n, factors.mkString(", "))

	def
	toString(nWidth: Int): String =
		defaultFormat.format(nWidth).format(n, factors.mkString(", "))

	def
	defaultFormat: String =
		"%%%ds | %%s"
}

object		Factorization
{
	def
	apply(n: Long): Factorization[Long] =
		new Factorization(n, Factorize.fermatFactorize(n): _*)

	def
	apply(n: BigInt): Factorization[BigInt] =
		new Factorization(n, Factorize.fermatFactorize(n): _*)
}


/** Like Factorization without the original composite number being recorded */
case
class		Factors[P : Integral] private (factors: PrimeFactors[P]*)
{
	val distinct		= factors map (_.p) distinct
	val factorCards		= Map( (factors map (pf => pf.p -> pf.n) ): _* )
	lazy val nFactors	= factorCards.values.sum

	def
	nDistinct: Int =
		factors.length

	def
	card(factor: P): Int =
		factorCards.getOrElse(factor, 0)

	def
	exp(pow: Int): Factors[P] = {
		val pOps = implicitly[Integral[P]]
		import pOps._
		Factors[P]((factors map (pf => PrimeFactors(pf.p, pf.n * pow))): _*)
	}

	def
	isPrime: Boolean =
		distinct.size == 1 && factors(0).n == 1

	override
	def
	toString: String =
		"{ %s }".format(factors.mkString(", "))
}

object		Factors
{
	def
	apply(n: Long): Factors[Long] =
		new Factors(Factorize.fermatFactorize(n): _*)

	def
	apply(n: BigInt): Factors[BigInt] =
		new Factors(Factorize.fermatFactorize(n): _*)
}



/**
	class Factorize: Integer Factorization
*/
object		Factorize
{
	import rrs.num.gcd
	import math._


	/* Trial Division */

	def
	factorizeTD(i: Int): Seq[PrimeFactors[Int]] = {
		val factors = new ArrayBuffer[PrimeFactors[Int]]
		val limit = sqrt(i).toInt
		var trial = 2
		var residue = i

		{
			var nReps = 0
			while (residue % trial == 0) {
				nReps += 1
				residue /= trial
			}
			if (nReps > 0)
				factors += new PrimeFactors(trial, nReps)
		}
		trial = 3

		while (residue > 1) {
			var nReps = 0
			while (residue % trial == 0) {
				nReps += 1
				residue /= trial
			}
			if (nReps > 0)
				factors += new PrimeFactors(trial, nReps)
			trial += 1
		}

		factors.toSeq
	}

	def
	factorizeTD(i: Long): Seq[PrimeFactors[Long]] = {
		val factors = new ArrayBuffer[PrimeFactors[Long]]
		val limit = sqrt(i).toLong
		var trial = 2l
		var residue = i

		{
			var nReps = 0
			while (residue % trial == 0) {
				nReps += 1
				residue /= trial
			}
			if (nReps > 0)
				factors += new PrimeFactors(trial, nReps)
		}
		trial = 3l

		while (residue > 1) {
			var nReps = 0
			while (residue % trial == 0) {
				nReps += 1
				residue /= trial
			}
			if (nReps > 0)
				factors += new PrimeFactors(trial, nReps)
			trial += 2
		}

		factors.toSeq
	}


	def
	factorizeTD(i: BigInt): Seq[PrimeFactors[BigInt]] = {
		val factors = new ArrayBuffer[PrimeFactors[BigInt]]
		val limit = BigInt(sqrt(i.toDouble).toLong)
		var trial = BI2
		var residue = i

		{
			var nReps = 0
			while (residue % trial == 0) {
				nReps += 1
				residue /= trial
			}
			if (nReps > 0)
				factors += new PrimeFactors(trial, nReps)
		}
		trial = BigInt(3)

		while (residue > 1) {
			var nReps = 0
			while (residue % trial == 0) {
				nReps += 1
				residue /= trial
			}
			if (nReps > 0)
				factors += new PrimeFactors(trial, nReps)
			trial += 2
		}

		factors.toSeq
	}


//	def
//	isPrime(n: Int): Boolean = {
//		val limit = ceil(sqrt(n)).toInt
//		var trial = 2
//		while (trial < limit) {
//			if (n % trial == 0)
//				return false
//			trial += 2
//		}
//
//		return true
//	}


	/* Fermat Factorization for Long */

	def
	fermatFactorize(N: Long): Seq[PrimeFactors[Long]] =
	(	for {
			fac <- fermatFactorize23(N).groupBy(identity)
		} yield {
			PrimeFactors(fac._1, fac._2.length)
		}
	).toSeq.sorted

	def
	fermatFactorize23(N: Long): List[Long] = {
		if (N == 1)
			Nil
		else
		if (N % 2 == 0)
			2 :: fermatFactorize23(N / 2)
		else
		if (N % 3 == 0)
			3 :: fermatFactorize23(N / 3)
		else
			fermatFactorizeR(N)
	}

	def
	fermatFactorizeR(N: Long): List[Long] =
		fermatFactorizeN(N) match {
			case (0, _) => N :: Nil
			case (s, t) => fermatFactorizeR(s) ++ fermatFactorizeR(t)
		}

	def
	fermatFactorizeN(N: Long): (Long, Long) = {
		var x = ceil(sqrt(N)).toLong
		while (x < N) {
			val ySq = x * x - N
			val y = rootIfSquare(ySq)
			if (y >= 0) {
				val s = x - y
				val t = x + y
				if (s != 1 && s != N)
					return (s, t)
			}
			x += 1
		}
		(0, 0)
	}

	private
	def
	rootIfSquare(x: Long): Long = {
		val r = ceil(sqrt(x)).toLong
		if (r * r == x)
			r
		else
			-1
	}

	def
	isPrime(n: Long): Boolean = {
		val factorization = fermatFactorize(n)
		factorization.length == 1 && factorization.head.n == 1
	}


	/* Fermat Factorization for BigInt */

	def
	fermatFactorize(N: BigInt): Seq[PrimeFactors[BigInt]] =
	(	for {
			fac <- fermatFactorize23(N).groupBy(identity)
		} yield {
			PrimeFactors(fac._1, fac._2.length)
		}
	).toSeq.sorted

	def
	fermatFactorize23(N: BigInt): List[BigInt] = {
		if (N == BI1)
			Nil
		else
		if (N % BI2 == 0)
			BI2 :: fermatFactorize23(N / BI2)
		else
		if (N % BI3 == 0)
			BI3 :: fermatFactorize23(N / BI3)
		else
			fermatFactorizeR(N)
	}

	def
	fermatFactorizeR(N: BigInt): List[BigInt] =
		fermatFactorizeN(N) match {
			case (BI0, _) => N :: Nil
			case (s, t) => fermatFactorizeR(s) ++ fermatFactorizeR(t)
		}

	def
	fermatFactorizeN(N: BigInt): (BigInt, BigInt) = {
		var x = BigInt(ceil(sqrt(N.toDouble)).toLong)
		while (x < N) {
			val ySq = x * x - N
			val y = rootIfSquare(ySq)
			if (y >= BI0) {
				val s = x - y
				val t = x + y
				if (s != BI1 && s != N)
					return (s, t)
			}
			x += 1
		}
		(BI0, BI0)
	}

	private
	def
	rootIfSquare(x: BigInt): BigInt = {
		val r = BigInt(ceil(sqrt(x.toDouble)).toLong)
		if (r * r == x)
			r
		else
			-1
	}

	def
	isPrime(n: BigInt): Boolean = {
		val factorization = fermatFactorize(n)
		factorization.length == 1 && factorization.head.n == 1
	}


	/* Pollard Rho Factorization for Long */

	var pollardRhoIterationMax = 100000
	var pollardRhoC = 1

	def
	pollardRho(N: Long): Seq[PrimeFactors[Long]] =
	(	for {
			fac <- pollardRho23(N).groupBy(identity)
		} yield {
			PrimeFactors(fac._1, fac._2.length)
		}
	).toSeq.sorted

	def
	pollardRho23(N: Long): List[Long] = {
		if (N == 1)
			Nil
		else
		if (N % 2 == 0)
			2 :: pollardRho23(N / 2)
		else
		if (N % 3 == 0)
			3 :: pollardRho23(N / 3)
		else
			pollardRhoR(N)
	}

	def
	pollardRhoR(N: Long): List[Long] =
		pollardRhoN(N) match {
			case (0, _) => N :: Nil
			case (s, t) => pollardRhoR(s) ++ pollardRhoR(t)
		}

	def
	pollardRhoN(N: Long): (Long, Long) = {
		val iLimit = pollardRhoIterationMax
		var iRemaining = iLimit
		var x_i = 2l
		var x_2i = 2l
//var i = 0
		while (iRemaining > 0) {
//printf("x_%d=%d%n", i, x_i); i += 1
			val x_iNext = x_i * x_i + pollardRhoC
			val x_2iNext = (x_2i * x_2i + pollardRhoC) * (x_2i * x_2i + pollardRhoC) + pollardRhoC
			x_i = x_iNext % N
			x_2i = x_2iNext % N
			val s = gcd(x_i - x_2i, N)
			if (s != 1 && s != N)
				return (s, N / s)
			iRemaining -= 1
		}
		(0, 0)
	}


	/* Pollard Rho Factorization for BigInt */

	def
	pollardRho(N: BigInt): Seq[PrimeFactors[BigInt]] =
	(	for {
			fac <- pollardRho23(N).groupBy(identity)
		} yield {
			PrimeFactors(fac._1, fac._2.length)
		}
	).toSeq.sorted

	def
	pollardRho23(N: BigInt): List[BigInt] = {
		if (N == BI1)
			Nil
		else
		if (N % BI2 == 0)
			BI2 :: pollardRho23(N / BI2)
		else
		if (N % BI3 == 0)
			BI3 :: pollardRho23(N / BI3)
		else
			pollardRhoR(N)
	}

	def
	pollardRhoR(N: BigInt): List[BigInt] =
		pollardRhoN(N) match {
			case (BI0, _) => N :: Nil
			case (s, t) => pollardRhoR(s) ++ pollardRhoR(t)
		}

	def
	pollardRhoN(N: BigInt): (BigInt, BigInt) = {
		val iLimit = pollardRhoIterationMax
		var iRemaining = iLimit
		var x_i = BI2
		var x_2i = BI2
		while (iRemaining > 0) {
			val x_iNext = x_i * x_i + pollardRhoC
			val x_21sqr = x_2i * x_2i + pollardRhoC
			val x_2iNext = x_21sqr + pollardRhoC
			x_i = x_iNext % N
			x_2i = x_2iNext % N
			val s = (x_i - x_2i).gcd(N)
			if (s != 1 && s != N)
				return (s, N / s)
			iRemaining -= 1
		}
		(BI0, BI0)
	}


	/* Brent's Factorization for Long */

	var brentFactorizeIterationMax = 100000

	def
	brentFactorize(N: Long): Seq[PrimeFactors[Long]] =
	(	for {
			fac <- brentFactorize23(N).groupBy(identity)
		} yield {
			PrimeFactors(fac._1, fac._2.length)
		}
	).toSeq.sorted

	def
	brentFactorize23(N: Long): List[Long] = {
		if (N == 1)
			Nil
		else
		if (N % 2 == 0)
			2 :: brentFactorize23(N / 2)
		else
		if (N % 3 == 0)
			3 :: brentFactorize23(N / 3)
		else
			brentFactorizeR(N)
	}

	def
	brentFactorizeR(N: Long): List[Long] =
		brentFactorizeN(N) match {
			case (0, _) => N :: Nil
			case (s, t) => brentFactorizeR(s) ++ brentFactorizeR(t)
		}

	def
	brentFactorizeN(N: Long): (Long, Long) = {
		var iRemaining = brentFactorizeIterationMax
		var x_i = 2l
		var x_m = 2l
		var i = 1l
		while (iRemaining > 0) {
			x_i = (x_i * x_i + 1) % N
			val s = gcd(x_i - x_m, N)
			if (s != 1 && s != N)
				return (s, N / s)
			if (isIntegralPow2(i))
				x_m = x_i
			iRemaining -= 1
		}
		(0, 0)
	}

	private
	def
	isIntegralPow2(z: Long): Boolean =
		(z & (z - 1)) == 0


	/* Brent's Factorization for BigInt */

	def
	brentFactorize(N: BigInt): Seq[PrimeFactors[BigInt]] =
	(	for {
			fac <- brentFactorize23(N).groupBy(identity)
		} yield {
			PrimeFactors(fac._1, fac._2.length)
		}
	).toSeq.sorted

	def
	brentFactorize23(N: BigInt): List[BigInt] = {
		if (N == BI1)
			Nil
		else
		if (N % BI2 == BI0)
			BI2 :: brentFactorize23(N / BI2)
		else
		if (N % BI3 == BI0)
			BI3 :: brentFactorize23(N / BI3)
		else
			brentFactorizeR(N)
	}

	def
	brentFactorizeR(N: BigInt): List[BigInt] =
		brentFactorizeN(N) match {
			case (BI0, _) => N :: Nil
			case (s, t) => brentFactorizeR(s) ++ brentFactorizeR(t)
		}

	def
	brentFactorizeN(N: BigInt): (BigInt, BigInt) = {
		var iRemaining = brentFactorizeIterationMax
		var x_i = BI2
		var x_m = BI2
		var i = BI1
		while (iRemaining > 0) {
			x_i = (x_i * x_i + 1) % N
			val s = (x_i - x_m).gcd(N)
			if (s != 1 && s != N)
				return (s, N / s)
			if (isIntegralPow2(i))
				x_m = x_i
			iRemaining -= 1
		}
		(BI0, BI0)
	}

	private
	def
	isIntegralPow2(z: BigInt): Boolean =
		(z & (z - 1)) == 0


	/* Pollard's P-1 Factorization for Long */

	var pollardP1IterationMax = 100000

	def
	pollardP1(N: Long): Seq[PrimeFactors[Long]] =
	(	for {
			fac <- pollardP123(N).groupBy(identity)
		} yield {
			PrimeFactors(fac._1, fac._2.length)
		}
	).toSeq.sorted

	def
	pollardP123(N: Long): List[Long] = {
		if (N == 1)
			Nil
		else
		if (N % 2 == 0)
			2l :: pollardP123(N / 2l)
		else
		if (N % 3 == 0)
			3l :: pollardP123(N / 3l)
		else
			pollardP1R(N)
	}

	def
	pollardP1R(N: Long): List[Long] =
		pollardP1N(N) match {
			case (0, _) => N :: Nil
			case (s, t) => pollardP1R(s) ++ pollardP1R(t)
		}

	def
	pollardP1N(N: Long): (Long, Long) = {
		var i = pollardP1IterationMax
		var k2fact = 1l
		var k = 1
		while (i > 0) {
//			k2fact = modPow(k2fact, k, N)
			k2fact = BigInt(k2fact).modPow(k, N).toLong
			val rk = gcd(k2fact - 1, N)
			if (rk != 1 && rk != N)
				return (rk, N / rk)
			k += 1
			i -= 1
		}
		(0, 0)
	}


	/* Pollard's P-1 Factorization for BigInt */

	def
	pollardP1(N: BigInt): Seq[PrimeFactors[BigInt]] =
	(	for {
			fac <- pollardP123(N).groupBy(identity)
		} yield {
			PrimeFactors(fac._1, fac._2.length)
		}
	).toSeq.sorted

	def
	pollardP123(N: BigInt): List[BigInt] = {
		if (N == BI1)
			Nil
		else
		if (N % BI2 == BI0)
			BI2 :: pollardP123(N / BI2)
		else
		if (N % BI3 == BI0)
			BI3 :: pollardP123(N / BI3)
		else
			pollardP1R(N)
	}

	def
	pollardP1R(N: BigInt): List[BigInt] =
		pollardP1N(N) match {
			case (BI0, _) => N :: Nil
			case (s, t) => pollardP1R(s) ++ pollardP1R(t)
		}

	def
	pollardP1N(N: BigInt): (BigInt, BigInt) = {
		var i = pollardP1IterationMax
		var k2fact = BI1
		var k = 1
		while (i > 0) {
			k2fact = k2fact.modPow(k, N)
			val rk = (k2fact - BI1).gcd(N)
			if (rk != 1 && rk != N)
				return (rk, N / rk)
			k += 1
			i -= 1
		}
		(BI0, BI0)
	}


	/* End of Factorization Methods */


	def
	nDivisors(n: Long): Long =
		(1l /: fermatFactorize(n)) { (nDiv: Long, factor: PrimeFactors[Long]) => nDiv * (factor.n + 1) }

	def
	nDivisors(n: BigInt): BigInt =
		(BI1 /: fermatFactorize(n)) { (nDiv: BigInt, factor: PrimeFactors[BigInt]) => nDiv * (factor.n + 1) }


	def
	sumDivisors(n: Long): Long =
		(	fermatFactorize(n) map { pf =>
				var pp = 1l
				((1 to pf.n) map { _: Int => pp *= pf.p; pp } sum) + 1
			}
		) product

	def
	sumDivisors(n: BigInt): BigInt =
		(	fermatFactorize(n) map { pf =>
				var pp = BI1
				((1 to pf.n) map { _: Int => pp *= pf.p; pp } sum) + 1
			}
		) product


	def
	isPerfect(n: Long): Boolean =
		sumDivisors(n) == 2 * n

	def
	isPerfect(n: BigInt): Boolean =
		sumDivisors(n) == BI2 * n


	def
	isDeficient(n: Long): Boolean =
		sumDivisors(n) - n < n

	def
	isDeficient(n: BigInt): Boolean =
		sumDivisors(n) - n < n


	def
	isAbundant(n: Long): Boolean =
		sumDivisors(n) - n > n

	def
	isAbundant(n: BigInt): Boolean =
		sumDivisors(n) - n > n


	/* BigInt constants */

	val BI0 = BigInt(0)
	val BI1 = BigInt(1)
	val BI2 = BigInt(2)
	val BI3 = BigInt(3)



	/* Tests */

	import scala.{BigInt => BI}

	import rrs.num.{PrimeFactors => PF}
	import PrimeFactors.num2PrimeFactors

	type Izer[P] = P => Seq[PF[P]]
	case class Problem[P : Integral](n: P, factors: PrimeFactors[P]*)
	case class Factorizer[P : Integral](name: String, izer: Izer[P])


	def
	runProblem[P](prob: Problem[P], factorizer: Factorizer[P]): Unit = {
		val factorization = factorizer.izer(prob.n)
		if (factorization sameElements prob.factors)
			printf("%7s   OK: %s%n", factorizer.name, factorization.mkString(", "))
		else
			printf("%7s fail: %s%n", factorizer.name, factorization.mkString(", "))
	}

	def
	runIzersL(prob: Problem[Long], which: String = "frpb"): Unit = {
		printf("N = %s:%n%14s%s%n", prob.n, " factors: ", prob.factors.mkString(", "))
		which foreach {
			case 'f' => runProblem(prob, Factorizer[Long]("Fermat", fermatFactorize _))
			case 'r' => runProblem(prob, Factorizer[Long]("Rho", pollardRho _))
			case 'p' => runProblem(prob, Factorizer[Long]("P-1", pollardP1 _))
			case 'b' => runProblem(prob, Factorizer[Long]("Brent", brentFactorize _))
			case _ =>
		}
		println
	}

	def
	runIzersB(prob: Problem[BI], which: String = "frpb"): Unit = {
		printf("N = %s:%n%14s%s%n", prob.n, " factors: ", prob.factors.mkString(", "))
		which foreach {
			case 'f' => runProblem(prob, Factorizer[BI]("Fermat", fermatFactorize _))
			case 'r' => runProblem(prob, Factorizer[BI]("Rho", pollardRho _))
			case 'p' => runProblem(prob, Factorizer[BI]("P-1", pollardP1 _))
			case 'b' => runProblem(prob, Factorizer[BI]("Brent", brentFactorize _))
			case _ =>
		}
		println
	}

	def
	runAll: Unit = {
		print("// Long\n")

		print("\n// Gras-Velazquez\n\n")
		runIzersL(Problem(248l, PF(2l, 3), 31l))
		runIzersL(Problem(93948l, PF(2l, 2), 3l, 7829l))
		runIzersL(Problem(3334502323l, 13003l, 256441l))
		runIzersL(Problem(35683941727333l, 151l, 263827l, 895729l))

		print("\n// A.1; page 41\n\n")
		runIzersL(Problem(3980021l, 1993l, 1997l))
		runIzersL(Problem(16831170221l, 129733l, 129737l))
		runIzersL(Problem(431589872009l, 656951l, 656959l))
		runIzersL(Problem(1469322167111l, 1212121l, 1212191l))

		print("\n// A.1; page 42\n\n")
		runIzersL(Problem(7956061979l, 1993l, 1997l, 1999l))
		runIzersL(Problem(110154695923l, 4789l, 4793l, 4799l))
		runIzersL(Problem(1019829472003l, 10061l, 10067l, 10069l))
		runIzersL(Problem(1005660644975291l, 100183l, 100189l, 100193l), "rpb")

		print("\n// A.1; page 42\n\n")
		runIzersL(Problem(14960418503l, 179l, 8467l, 9871l), "rpb")
		runIzersL(Problem(8355211084777l, 1163l, 12347l, 581857l), "rpb")

		print("\n// BI\n")

		print("\n// Gras-Velazquez\n\n")
		runIzersB(Problem(BI(248l), PF(BI(2l), 3), BI(31l)))
		runIzersB(Problem(BI(93948l), PF(BI(2l), 2), BI(3l), BI(7829l)))
		runIzersB(Problem(BI(3334502323l), BI(13003l), BI(256441l)))
		runIzersB(Problem(BI(35683941727333l), BI(151l), BI(263827l), BI(895729l)))

		print("\n// A.1; page 41\n\n")
		runIzersB(Problem(BI(3980021l), BI(1993l), BI(1997l)))
		runIzersB(Problem(BI(16831170221l), BI(129733l), BI(129737l)))
		runIzersB(Problem(BI(431589872009l), BI(656951l), BI(656959l)))
		runIzersB(Problem(BI(1469322167111l), BI(1212121l), BI(1212191l)))

		print("\n// A.1; page 42\n\n")
		runIzersB(Problem(BI(7956061979l), BI(1993l), BI(1997l), BI(1999l)))
		runIzersB(Problem(BI(110154695923l), BI(4789l), BI(4793l), BI(4799l)))
		runIzersB(Problem(BI(1019829472003l), BI(10061l), BI(10067l), BI(10069l)))
		runIzersB(Problem(BI(1005660644975291l), BI(100183l), BI(100189l), BI(100193l)), "rpb")

		print("\n// A.1; page 42\n\n")
		runIzersB(Problem(BI(14960418503l), BI(179l), BI(8467l), BI(9871l)), "rpb")
		runIzersB(Problem(BI(8355211084777l), BI(1163l), BI(12347l), BI(581857l)), "rpb")
		runIzersB(Problem(BI("416531649825896503"), BI(12983l), BI(987533l), BI(32487877l)), "rpb")
		runIzersB(Problem(BI("153674304751986405509"), BI(762479l), BI(1276237l), BI(157921783l)), "rpb")
		println
	}
}
