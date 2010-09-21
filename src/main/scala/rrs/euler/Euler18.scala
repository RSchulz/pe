/*
	Euler18.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	By starting at the top of the triangle below and moving to adjacent numbers
	on the row below, the maximum total from top to bottom is 23.

			   3
			  7 4
			 2 4 6
			8 5 9 3

	That is, 3 + 7 + 4 + 9 = 23.

	Find the maximum total from top to bottom of the triangle below:

										75
									  95  64
									17  47  82
								  18  35  87  10
								20  04  82  47  65
							  19  01  23  75  03  34
							88  02  77  73  07  63  67
						  99  65  04  28  06  16  70  92
						41  41  26  56  83  40  80  70  33
					  41  48  72  33  47  32  37  16  94  29
					53  71  44  65  25  43  91  52  97  51  14
				  70  11  33  28  77  73  17  78  39  68  17  57
				91  71  52  38  17  14  91  43  58  50  27  29  48
			  63  66  04  68  89  53  67  30  73  16  69  87  40  31
			04  62  98  27  23  09  70  98  73  93  38  53  60  04  23

	NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route.
	However, Problem 67, is the same challenge with a triangle containing one-hundred rows;
	it cannot be solved by brute force, and requires a clever method! ;o)
*/

package		rrs.euler


/**
	object Euler18
*/
object		Euler18
{
	val triangle =
		Array(  Array                            (75),								//  0
				Array                          (95, 64),							//  1
				Array                        (17, 47, 82),							//  2
				Array                      (18, 35, 87, 10),						//  3
				Array                    (20,  4, 82, 47, 65),						//  4
				Array                  (19,  1, 23, 75,  3, 34),					//  5
				Array                (88,  2, 77, 73,  7, 63, 67),					//  6
				Array              (99, 65,  4, 28,  6, 16, 70, 92),				//  7
				Array            (41, 41, 26, 56, 83, 40, 80, 70, 33),				//  8
				Array          (41, 48, 72, 33, 47, 32, 37, 16, 94, 29),			//  9
				Array        (53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14),			// 10
				Array      (70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57),		// 11
				Array    (91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48),		// 12
				Array  (63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31),	// 13
				Array(04, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23))	// 14

	val triHeight = triangle.length
	val triBottom = triHeight - 1


//	def
//	pBits(p: Int): Int =
//		p << 1

	def
	triElement(pBits: Int, d: Int): Int =
		triangle(d)(pathIndex(pBits, d))

	def
	pathIndex(pBits: Int, d: Int): Int =
		(0 /: enumeratePath(pBits, d)) { (accum: Int, pBit: Int) => accum + pBit }

	def
	enumeratePath(pBits: Int, d: Int): List[Int] =
		eP(pBits, 0, d)

	def
	eP(path: Int, index: Int, max: Int): List[Int] =
		if (index <= max)
			((path >> index) & 1) :: eP(path, index + 1, max)
		else
			Nil

	def
	triRow(d: Int): List[Int] =
		(for (p <- 0 until (1 << (d + 1)) by 2) yield { triElement(p, d) }).toList

	def
	triPath(pBits: Int, d: Int): Seq[Int] =
		(0 to d) map (triElement(pBits, _))

	def
	sumPath(pBits: Int, d: Int): Int =
		triPath(pBits, d) sum

	def
	euler18S: Int =
		(0 to (1 << triBottom) by 2) map(sumPath(_, triBottom)) max

	def
	euler18: (Int, Int) =
	(	(0 to ((1 << triHeight) - 1) by 2)
		.map { path => (path, sumPath(path, triBottom)) }
	)	.max(Ordering.by((pi: (Int, Int)) => pi._2))

	def
	showAllPaths: Unit =
		(	(0 to ((1 << triHeight) - 1) by 2)
			.map { path => (path, triPath(path, triBottom), sumPath(path, triBottom)) }
		) foreach { pInfo =>
			printf("  path=%#5x%n", pInfo._1)
			printf(" pVals=%s%n", pInfo._2.mkString(", "))
			printf("  pSum=%d%n", pInfo._3)
		}

//	def
//	main(args: Array[String]): Unit =
//		printf("Euler18: %d%n", euler18S)

	def
	main(args: Array[String]): Unit = {
		val answer = euler18
		printf("Euler18: path=%s%n       values=%s%n          sum=%d%n",
			   enumeratePath(answer._1, triBottom).mkString(", "),
			   triPath(answer._1, triBottom).mkString(", "),
			   answer._2)
	}
}
