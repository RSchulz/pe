/*
	Fibonacci.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

package		rrs.num


/**
	class FibonacciL: Long Integer Fibonacci Numbers
*/
object		FibonacciL
{
	def
	fibs: Stream[Long] =
		1l #:: 1l #:: fibs.zip(fibs.tail).map(p => p._1 + p._2)
}


/**
	class FibonacciBI: BigInt Fibonacci Numbers
*/
object		FibonacciBI
{
	private val BI1 = BigInt(1)

	def
	fibs: Stream[BigInt] =
		BI1 #:: BI1 #:: fibs.zip(fibs.tail).map(p => p._1 + p._2)
}
