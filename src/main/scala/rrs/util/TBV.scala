/*
	TBV.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

package		rrs.util


abstract
class		TBV



/**
	class TBVI: Trivial Bit Vector with Int Indexes
*/
final
class		TBVI(val nBits: Int)
extends		TBV
{
	private val nSlots = nBits / 32 + 1

	require(nSlots < Int.MaxValue)

	protected val bits = new Array[Int](nSlots.toInt)

	var inverted = false

	def
	apply(i: Int): Boolean =
		if (inverted) (bits(i / 0x20) & (1 << (i & 0x1f))) == 0
		else          (bits(i / 0x20) & (1 << (i & 0x1f))) != 0

	def
	update(i: Int, b: Boolean): Unit =
		if (b) bits(i / 0x20) |=  (1 << (i & 0x1f))
		else   bits(i / 0x20) &= ~(1 << (i & 0x1f))


	def
	nextSet(i: Int): Int = {
		var scan = i + 1
		while (scan < nBits && !this(scan)) scan += 1

		if (scan < nBits) scan
		else              -1
	}

	def
	nextClear(i: Int): Int = {
		var scan = i + 1
		while (scan < nBits && this(scan)) scan += 1

		if (scan < nBits) scan
		else              -1
	}

	def
	next(i: Int, v: Boolean): Int = {
		var scan = i + 1
		if (v) while (scan < nBits && !this(scan)) scan += 1
		else   while (scan < nBits &&  this(scan)) scan += 1

		if (scan < nBits) scan
		else              -1
	}


	def
	prevSet(i: Int): Int = {
		var scan = i - 1
		while (scan >= 0 && !this(scan)) scan -= 1

		if (scan >= 0) scan
		else           -1
	}

	def
	prevClear(i: Int): Int = {
		var scan = i - 1
		while (scan >= 0 && this(scan)) scan -= 1

		if (scan < nBits) scan
		else              -1
	}

	def
	prev(i: Int, v: Boolean): Int = {
		var scan = i - 1
		if (v)	while (scan >= 0 && !this(scan)) scan -= 1
		else	while (scan >= 0 &&  this(scan)) scan -= 1

		if (scan >= 0) scan
		else           -1
	}


	def
	find(i: Int, v: Boolean, skip: Int): Int = {
		var scan = i
		if (v) while (scan >= 0 && scan < nBits && !this(scan)) scan += skip
		else   while (scan >= 0 && scan < nBits &&  this(scan)) scan += skip

		if (scan >= 0 && scan < nBits) scan
		else                           -1
	}
}


/**
	class TBVL: Trivial Bit Vector with Long Indexes
*/
final
class		TBVL(val nBits: Long)
extends		TBV
{
	private val nSlots = nBits / 0x40 + 1

	require(nSlots < Int.MaxValue)

	protected val bits = new Array[Long](nSlots.toInt)

	var inverted = false


	def
	apply(i: Int): Boolean =
		if (inverted) (bits(i / 0x40) & (1l << (i & 0x3f))) == 0
		else          (bits(i / 0x40) & (1l << (i & 0x3f))) != 0

	def
	apply(i: Long): Boolean =
		if (inverted) (bits((i / 0x40l).toInt) & (1l << (i & 0x3f))) == 0
		else          (bits((i / 0x40l).toInt) & (1l << (i & 0x3f))) != 0


	def
	update(i: Int, b: Boolean): Unit =
		if (b) bits(i / 0x40) |=  (1l << (i & 0x3f))
		else   bits(i / 0x40) &= ~(1l << (i & 0x3f))

	def
	update(i: Long, b: Boolean): Unit =
		if (b) bits((i / 0x40l).toInt) |=  (1l << (i & 0x3f))
		else   bits((i / 0x40l).toInt) &= ~(1l << (i & 0x3f))


	def
	nextSet(i: Long): Long = {
		var scan = i + 1
		while (scan < nBits && !this(scan)) scan += 1

		if (scan < nBits) scan
		else              -1
	}


	def
	nextClear(i: Long): Long = {
		var scan = i + 1
		while (scan < nBits && this(scan)) scan += 1

		if (scan < nBits) scan
		else              -1
	}


	def
	next(i: Long, v: Boolean): Long = {
		var scan = i + 1
		if (v) while (scan < nBits && !this(scan)) scan += 1
		else   while (scan < nBits &&  this(scan)) scan += 1

		if (scan < nBits)
			scan
		else
			-1
	}


	def
	prevSet(i: Long): Long = {
		var scan = i - 1
		while (scan >= 0 && !this(scan)) scan -= 1

		if (scan >= 0) scan
		else           -1
	}

	def
	prevClear(i: Long): Long = {
		var scan = i - 1
		while (scan >= 0 && this(scan)) scan -= 1

		if (scan >= 0) scan
		else           -1
	}

	def
	prev(i: Long, v: Boolean): Long = {
		var scan = i - 1
		if (v) while (scan >= 0 && !this(scan)) scan -= 1
		else   while (scan >= 0 &&  this(scan)) scan -= 1

		if (scan >= 0) scan
		else           -1
	}


	def
	find(i: Long, v: Boolean, skip: Long): Long = {
		var scan = i + skip
		if (v) while (scan >= 0 && scan < nBits && !this(scan)) scan += skip
		else   while (scan >= 0 && scan < nBits &&  this(scan)) scan += skip

		if (scan >= 0 && scan < nBits) scan
		else                           -1
	}
}


/**
	class TBVBI: Trivial Bit Vector with BigInt Indexes
*/
sealed
class		TBVBI(val nBits: BigInt)
extends		TBV
{
	def
	this(nBits: Long) =
		this(BigInt(nBits))

	def
	this(nBits: Int) =
		this(BigInt(nBits))


	private val nSlots = nBits / BigInt(64) + 1

	require(nSlots < Int.MaxValue)

	private val bits = new Array[Long](nSlots.toInt)


	def
	apply(i: Int): Boolean =
		(bits(i / 0x40) & (1 << (i & 0x3f))) != 0

	def
	apply(i: Long): Boolean =
		(bits((i / 0x40l).toInt) & (1 << (i & 0x3f))) != 0

	def
	apply(i: BigInt): Boolean =
		(bits((i.toLong / 0x40l).toInt) & (1 << (i.toLong & 0x3f))) != 0


	def
	update(i: Int, b: Boolean): Unit =
		if (b)
			bits(i / 0x40) |=  (1 << (i & 0x3f).toInt)
		else
			bits(i / 0x40) &= ~(1 << (i & 0x3f).toInt)

	def
	update(i: Long, b: Boolean): Unit =
		if (b)
			bits((i / 0x40l).toInt) |=  (1 << (i & 0x3f))
		else
			bits((i / 0x40l).toInt) &= ~(1 << (i & 0x3f))

	def
	update(i: BigInt, b: Boolean): Unit = {
		if (b)
			bits((i / 0x40l).toInt) |=  (1 << (i % 0x40).toInt)
		else
			bits((i / 0x40l).toInt) &= ~(1 << (i % 0x40).toInt)
	}


	def
	nextSet(i: Long): BigInt =
		nextSet(BigInt(i))

	def
	nextSet(i: BigInt): BigInt = {
		var scan = i + 1
		while (scan < nBits && !this(scan)) scan += 1

		if (scan < nBits) scan
		else              -1
	}


	def
	nextClear(i: Long): BigInt =
		nextClear(BigInt(i))

	def
	nextClear(i: BigInt): BigInt = {
		var scan = i + 1
		while (scan < nBits && this(scan)) scan += 1

		if (scan < nBits) scan
		else              -1
	}


	def
	next(i: Long, v: Boolean): BigInt =
		next(BigInt(i), v)

	def
	next(i: BigInt, v: Boolean): BigInt = {
		var scan = i + 1
		if (v) while (scan < nBits && !this(scan)) scan += 1
		else   while (scan < nBits &&  this(scan)) scan += 1

		if (scan < nBits) scan
		else              -1
	}


	def
	prevSet(i: Long): BigInt =
		prevSet(BigInt(i))

	def
	prevSet(i: BigInt): BigInt = {
		var scan = i - 1
		while (scan >= 0 && !this(scan)) scan -= 1

		if (scan >= 0) scan
		else           -1
	}

	def
	prevClear(i: Long): BigInt =
		prevClear(BigInt(i))

	def
	prevClear(i: BigInt): BigInt = {
		var scan = i - 1
		while (scan >= 0 && this(scan)) scan -= 1

		if (scan >= 0) scan
		else           -1
	}

	def
	prev(i: Long, v: Boolean): BigInt =
		prev(BigInt(i), v)

	def
	prev(i: BigInt, v: Boolean): BigInt = {
		var scan = i - 1
		if (v) while (scan >= 0 && !this(scan)) scan -= 1
		else   while (scan >= 0 &&  this(scan)) scan -= 1

		if (scan >= 0) scan
		else           -1
	}


	def
	find(i: Long, v: Boolean, skip: Long): BigInt =
		find(BigInt(i), v, BigInt(skip))

	def
	find(i: BigInt, v: Boolean, skip: BigInt): BigInt = {
		var scan = i + skip
		if (v) while (scan >= 0 && scan < nBits && !this(scan)) scan += skip
		else   while (scan >= 0 && scan < nBits &&  this(scan)) scan += skip

		if (scan >= 0 && scan < nBits) scan
		else                           -1
	}
}
