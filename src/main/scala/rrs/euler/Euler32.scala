/*
	Euler32.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	We shall say that an n-digit number is pandigital if it makes use
	of all the digits 1 to n exactly once; for example, the 5-digit
	number, 15234, is 1 through 5 pandigital.

	The product 7254 is unusual, as the identity, 39 × 186 = 7254,
	containing multiplicand, multiplier, and product is 1 through 9 pandigital.

	Find the sum of all products whose multiplicand/multiplier/product
	identity can be written as a 1 through 9 pandigital.

	HINT:	Some products can be obtained in more than one way
			so be sure to only include it once in your sum.
*/


package		rrs.euler


/**
	class Euler32
*/
object		Euler32
{
	import rrs.num._
	import Permute._

	import collection.mutable.{Set => MSet, HashSet => MHashSet}

	type Perm	= Array[Int]
	type Split	= (Int, Int, Int)
	type MMP	= (Int, Int, Int)

	val splits = ( for {
						a	<- 1 to (9 - 2)
						b	<- 1 to (9 - a - 1)
						c	<- 1 to (9 - b)
						if	a + b + c == 9
					} yield (a, b, c) )


	def
	collectMMPIdentities(mmpi: MSet[Int]): Unit = {
		val perm = perm1(9)
		while (advancePerm(perm))
			mmpi ++= splits flatMap(isMMPIdentity(perm, _))
	}

	def
	isMMPIdentity(perm: Perm, split: Split): Option[Int] = {
		val mmp = mmpPerm(perm, split)
		if (mmp._1 * mmp._2 == mmp._3)
			Some(mmp._3)
		else
			None
	}

	def
	mmpPerm(perm: Perm, split: Split): MMP =
		(field(perm, 0,                   split._1),
		 field(perm, split._1,            split._1 + split._2),
		 field(perm, split._1 + split._2, split._1 + split._2 + split._3))

	def
	field(perm: Perm, from: Int, to: Int): Int =
		(0 /: perm.slice(from, to))(_ * 10 + _)


	def
	euler32: Int = {
		val mmpIdentities = new MHashSet[Int]
		collectMMPIdentities(mmpIdentities)
		mmpIdentities sum
	}

	def
	main(args: Array[String]): Unit =
		printf("Euler32: %d%n", euler32)
}
