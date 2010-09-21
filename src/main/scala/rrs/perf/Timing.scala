/*
	Timing.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

package		rrs.perf

import		scala.collection.IndexedSeq

import		java.lang.System.{currentTimeMillis => now}


trait		Work[R]
{
	def work: R
}


/**
	object Timing
*/
object		Timing
{
	import rrs.num.Stats.msd


	def
	time1[R](work: => R): (Double, R) = {
		val startTime = now
		val result: R = work
		((now - startTime).toDouble / 1000.0d, result)
	}

	def
	time1[R](nWarmup: Int, work: => R): (Double, R) = {
		(1 to nWarmup) foreach { _ => work }
		val startTime = now
		val result: R = work
		((now - startTime).toDouble / 1000.0d, result)
	}


	def
	timeN[R](n: Int, work: => R): (Double, IndexedSeq[(Double, R)]) = {
		val startTime = now
		val results = for (i <- 1 to n) yield {
			val startOne = now
			val result = work
			((now - startOne).toDouble / 1000.0d, result)
		}
		((now - startTime).toDouble / 1000.0d, results)
	}

	def
	timeN[R](nWarmup: Int, n: Int, work: => R): (Double, IndexedSeq[(Double, R)]) = {
		(1 to nWarmup) foreach { _ => work }
		val startTime = now
		val results = for (i <- 1 to n) yield {
			val startOne = now
			val result = work
			((now - startOne).toDouble / 1000.0d, result)
		}
		((now - startTime).toDouble / 1000.0d, results)
	}


	def
	reportN[R](n: Int, work: => R): (Double, IndexedSeq[(Double, R)]) = {
//		val timings = timeN(n, work)

		val startTime = now
		val results = for (i <- 1 to n) yield {
			val startOne = now
			val result = work
			val timing = ((now - startOne).toDouble / 1000.0d, result)
			printf("%8.3f: %s%n", timing._1, timing._2)
			timing
		}
		val timings = ((now - startTime).toDouble / 1000.0d, results)

//		timings._2 foreach { timing => printf("%8.3f: %s%n", timing._1, timing._2) }

		val stats = msd(timings._2.map(_._1))
		printf("%8.3f total; %4.3f mean; %4.3f std dev%n%n", timings._1, stats._2, stats._3)

		timings
	}

	def
	reportN[R](nWarmup: Int, n: Int, work: => R): (Double, IndexedSeq[(Double, R)]) = {
//		val timings = timeN(nWarmup, n, work)

		(1 to nWarmup) foreach { _ => work }

		val startTime = now
		val results = for (i <- 1 to n) yield {
			val startOne = now
			val result = work
			val timing = ((now - startOne).toDouble / 1000.0d, result)
			printf("%8.3f: %s%n", timing._1, timing._2)
			timing
		}
		val timings = ((now - startTime).toDouble / 1000.0d, results)

//		timings._2 foreach { timing => printf("%8.3f: %s%n", timing._1, timing._2) }

		val stats = msd(timings._2.map(_._1))
		printf("%8.3f total; %4.3f mean; %4.3f std dev%n%n", timings._1, stats._2, stats._3)

		timings
	}


	def
	reportN[R](n: Int, work: Work[R]): (Double, IndexedSeq[(Double, R)]) = {
//		val timings = timeN(n, work)

		val startTime = now
		val results = for (i <- 1 to n) yield {
			val startOne = now
			val result = work.work
			val timing = ((now - startOne).toDouble / 1000.0d, result)
			printf("%8.3f: %s%n", timing._1, timing._2)
			timing
		}
		val timings = ((now - startTime).toDouble / 1000.0d, results)

//		timings._2 foreach { timing => printf("%8.3f: %s%n", timing._1, timing._2) }

		val stats = msd(timings._2.map(_._1))
		printf("%8.3f total; %4.3f mean; %4.3f std dev%n%n", timings._1, stats._2, stats._3)

		timings
	}

	def
	reportN[R](nWarmup: Int, n: Int, work: Work[R]): (Double, IndexedSeq[(Double, R)]) = {
//		val timings = timeN(nWarmup, n, work)

		(1 to nWarmup) foreach { _ => work.work }

		val startTime = now
		val results = for (i <- 1 to n) yield {
			val startOne = now
			val result = work.work
			val timing = ((now - startOne).toDouble / 1000.0d, result)
			printf("%8.3f: %s%n", timing._1, timing._2)
			timing
		}
		val timings = ((now - startTime).toDouble / 1000.0d, results)

//		timings._2 foreach { timing => printf("%8.3f: %s%n", timing._1, timing._2) }

		val stats = msd(timings._2.map(_._1))
		printf("%8.3f total; %4.3f mean; %4.3f std dev%n%n", timings._1, stats._2, stats._3)

		timings
	}
}
