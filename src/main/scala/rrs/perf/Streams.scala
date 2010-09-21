/*
	Streams.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

package		rrs.perf

import		rrs.perf.Timing._

import		java.lang.System.err.{printf => ePrintf}


class		LongsWork(val nLongs: Int)
extends		Work[Long]
{
	def
	work: Long =
		ints(0) dropWhile (_ < nLongs) head

	def
	ints(n: Long): Stream[Long] =
		Stream.cons(n, ints(n + 1))
}


object		StreamMark
{
	val progName	= "StreamMarkLW"

	val OneIntRE	= "([0-9]+)".r
	val TwoIntRE	= "([0-9]+),([0-9]+)".r
	val ThreeIntRE	= "([0-9]+),([0-9]+),([0-9]+)".r
	val OptionRE	= "(-.*)".r

	def
	main(args: Array[String]): Unit = {
		if (args.length == 0)
			ePrintf("%s: Usage: %s [ count | rep,count | skip,rep,count ] ...%n",
					progName, progName)

		var fl = true

		args foreach { arg =>
			arg match {
				case "--fl" => fl = true
				case "--lw" => fl = false

				case OptionRE(option) => ePrintf("%s: Option \"%s\" not recognized%n", option)

				case OneIntRE(count) =>
					if (fl) {
						val iCount = count.toInt
						reportN(1, ints(0) dropWhile (_ < iCount) head)
					}
					else
						reportN(1, new LongsWork(count.toInt))

				case TwoIntRE(rep, count) =>
					if (fl) {
						val iCount = count.toInt
						reportN(rep.toInt, ints(0) dropWhile (_ < iCount) head)
					}
					else
						reportN(rep.toInt, new LongsWork(count.toInt))

				case ThreeIntRE(skip, rep, count) =>
					if (fl) {
						val iCount = count.toInt
						reportN(skip.toInt, rep.toInt, ints(0) dropWhile (_ < iCount) head)
					}
					else
						reportN(skip.toInt, rep.toInt, new LongsWork(count.toInt))

				case _ => ePrintf("%s: Argument \"%s\" not valid -- ignored%n", progName, arg)
			}
		}
	}

	def
	ints(n: Long): Stream[Long] =
		Stream.cons(n, ints(n + 1))
}
