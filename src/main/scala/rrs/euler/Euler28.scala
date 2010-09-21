/*
	Euler28.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	Starting with the number 1 and moving to the right in a
	clockwise direction a 5 by 5 spiral is formed as follows:

			21 22 23 24 25
			20  7  8  9 10
			19  6  1  2 11
			18  5  4  3 12
			17 16 15 14 13

	It can be verified that the sum of the numbers on the diagonals is 101.

	What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
*/

package		rrs.euler


/**
	class Euler28
*/
object		Euler28
{
	/*
	   2                                              3
		  111 112 113 114 115 116 117 118 119 120 121
		  110  73  74  75  76  77  78  79  80  81  82
		  109  72  43  44  45  46  47  48  49  50  83
		  108  71  42  21  22  23  24  25  26  51  84
		  107  70  41  20   7   8   9  10  27  52  85
		  106  69  40  19   6   1   2  11  28  53  86
		  105  68  39  18   5   4   3  12  29  54  87
		  104  67  38  17  16  15  14  13  30  55  88
		  103  66  37  36  35  34  33  32  31  56  89
		  102  65  64  63  62  61  60  59  58  57  90
		  101 100  99  98  97  96  95  94  93  92  91
	   1                                              0

	           0   1   2   3   4   5
	       _________________________
	      0 |  1   3  13  31  57  91
	      1 |  1   5  17  37  65 101
	      2 |  1   7  21  43  73 111
	      3 |  1   9  25  49  81 121

		 C(R) = R * 8
		   0  =  0
		   1  =  8
		   2  = 16
		   3  = 24
		   4  = 32
	*/

	def
	triangle8(n: Long): Long =
		(n * n + n) * 4

	def
	diag(radius: Long, phase: Long): Long = {
		require(radius >= 0)
		require(phase >= 0 && phase < 4)

		if (radius == 0)
			1
		else
			phase match {
				case 0 => 1 + triangle8(radius) - radius * 6
				case 1 => 1 + triangle8(radius) - radius * 4
				case 2 => 1 + triangle8(radius) - radius * 2
				case 3 => 1 + triangle8(radius)
			}
	}

	def
	euler28: Long =
		((for (phase <- 0 to 3; radius <- 1 to 500) yield diag(radius, phase)) sum) + 1

	def
	main(args: Array[String]): Unit =
		printf("Euler28: %d%n", euler28)
}
