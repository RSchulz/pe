/*
	Permute.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

package		rrs.num


/**
	object Permute
*/
object		Permute
{
	/** Create a stream that yields lexicographically increasing permutations of length <i>n</i> */
	def
	permutations(n: Int): Stream[Array[Int]] =
		permStream(perm0(n))

	def
	permStream(perm: Array[Int]): Stream[Array[Int]] =
		perm #:: (
			if (isLastPerm(perm))
				Stream.empty
			else
				permStream(nextPerm(perm))
		)


	/**
		Alter the permutation <i>perm</i> to be the lexicographically subsequent permutation, if any.
		Return <b>true</b> if <i>perm<i/> was not the lexicographically terminal permutation, false otherwise.
	*/
	def
	advancePerm(perm: Array[Int]): Boolean = {
		val l = perm.length

		/* Find the "tail," the terminal subsequence that is in strictly descending order */
		var iTail = l - 1
		while (iTail > 0 && perm(iTail) < perm(iTail - 1))
			iTail -= 1

		/* If the whole sequence is in descending order (or has only one element), it is the last permutation */
		if (iTail == 0)
			false

		else {
			/* Find the smallest element in the tail larger than the element just before the tail */
			val headLast = perm(iTail - 1)
			var tailMin = l + 1
			var iSwap = -1
			var iScan = iTail

			while (iScan < l) {
				val s = perm(iScan)
				if (s > headLast && s < tailMin) {
					tailMin = s
					iSwap = iScan
				}
				iScan += 1
			}

			perm(iTail - 1) = tailMin
			perm(iSwap) = headLast

			/* Reverse the new tail, the portion after iSwap */
			var iRevL = iTail
			var iRevR = l - 1
			while (iRevL < iRevR) {
				val r = perm(iRevR)
				perm(iRevR) = perm(iRevL)
				perm(iRevL) = r
				iRevL += 1
				iRevR -= 1
			}

			true
		}
	}

	def
	isLastPerm(perm: Array[Int]): Boolean = {
		val l = perm.length
		if (l < 2)
			true
		else {
			/* Find the "tail," the terminal subsequence that is in strictly descending order */
			var iTail = l - 1
			while (iTail > 0 && perm(iTail) < perm(iTail - 1))
				iTail -= 1

			/* If the whole sequence is in descending order, it is the last permutation */
			iTail == 0
		}
	}

	/**
		Compute the permutation that lexicographically follows <i>perm</i>,
		if any, returning (a copy of) <i>perm</> if none.
	*/
	def
	nextPerm(perm: Array[Int]): Array[Int] = {
		val l = perm.length
		val next = new Array[Int](l)
		Array.copy(perm, 0, next, 0, l)
		advancePerm(next)
		next
	}

	/** Create the lexicographically minimal permutation of <i>n</i> integers i such that 0 ≤ i < n */
	def
	perm0(n: Int): Array[Int] =
		Array.iterate[Int](0, n)(_ + 1)

	/** Create the lexicographically minimal permutation of <i>n</i> integers i such that 1 ≤ i ≤ n */
	def
	perm1(n: Int): Array[Int] =
		Array.iterate[Int](1, n)(_ + 1)
}
