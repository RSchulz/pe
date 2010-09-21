/*
	Euler81.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right,
	by only moving to the right and down, is indicated in bold red and is equal to 2427.

		131	673	234	103	18
		^^^
		201	96	342	965	150
		^^^ ^^  ^^^
		630	803	746	422	111
		        ^^^ ^^^
		537	699	497	121	956
		            ^^^
		805	732	524	37	331
		            ^^  ^^^

	Find the minimal path sum, in matrix.txt (<http://projecteuler.net/project/matrix.txt>),
	a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by
	only moving right and down.
*/

package		rrs.euler



/**
	object Euler81
*/
object		Euler81
{
	import	io.Source
	import	collection.mutable.ArrayBuffer


	type	Path		= Vector[Coord]


	class	Coord(r: Int, c: Int) {
		private
		val rowCol	= r << 16 | c

		def row: Int = rowCol >> 16
		def col: Int = rowCol & 0xffff

		override
		def
		toString: String =
			"[r%d, c%d]".format(row, col)
	}

	object	Coord
	{
		def
		apply(row: Int, col: Int): Coord =
			new Coord(row, col)
	}

	case
	class	Matrix(m: Array[Array[Int]])
	{
		val height	= m.length
		val width	= m(0).length

		lazy
		val size	= { assert(height == width, "Matrix size not defined if width != height"); width }

		def
		apply(row: Int): Array[Int] =
			m(row)

		def
		apply(row: Int, col: Int): Int =
			m(row)(col)

		def
		show(fmt: String = " %d "): Unit = {
			for (r <- 0 until height) {
				for (c <- 0 until width)
					printf(fmt, this(r, c))
				println
			}

		}

//		def
//		udpate(row: Int, col: Int, v: Int): Unit =
//			m(row)(col) = v
	}

	object	Matrix
	{
		def
		apply(size: Int): Matrix =
			new Matrix(Array.ofDim[Int](size, size))
	}


	case
	class	MPath private (matrix: Matrix, coords: Path, sum: Int)
	{
		def
		advance(coord: Coord): MPath =
			MPath(matrix, coords :+ coord, sum + matrix(coord.row)(coord.col))

		def
		left: MPath = {
			val term = terminus
			advance(Coord(term.row, term.col - 1))
		}

		def
		right: MPath = {
			val term = terminus
			advance(Coord(term.row, term.col + 1))
		}

		def
		up: MPath = {
			val term = terminus
			advance(Coord(term.row - 1, term.col))
		}

		def
		down: MPath = {
			val term = terminus
			advance(Coord(term.row + 1, term.col))
		}


		val height					= matrix.height
		val width					= matrix.width

		def atTop: Boolean			= terminus.row == 0
		def atLeft: Boolean			= terminus.col == 0
		def atTopLeft: Boolean		= atTop && atLeft

		def atBottom: Boolean		= terminus.row == height - 1
		def atRight: Boolean		= terminus.col == width - 1
		def atBottomRight: Boolean	= atBottom && atRight

		def origin: Coord			= coords.head
		def terminus: Coord			= coords.last
		def pathLength: Int			= coords.length - 1


		override
		def
		toString: String =
			"{%d via %s}".format(sum, coords.mkString("[", ",", "]"))
	}

	object	MPath
	{
		def
		origin(matrix: Matrix): MPath =
			MPath(matrix, Vector(Coord(0, 0)), matrix(0)(0))

		def
		from(matrix: Matrix, coord: Coord): MPath =
			MPath(matrix, Vector(coord), matrix(coord.row, coord.col))
	}


	def
	bestSum(matrix: Matrix): Int = {
		val mLimit = matrix.size - 1
		val tlSums = Matrix(matrix.width)
		val brSums = Matrix(matrix.width)

		bestSumsTL(matrix, tlSums, 0, mLimit)

//		println("TL Sums:")
//		tlSums.show(" %4d ")
//		println

		bestSumsBR(matrix, brSums, mLimit * 2, mLimit)

//		println("\nBR Sums:")
//		brSums.show(" %4d ")
//		println

		( ( mLimit to 0 by -1) map  { row =>
										val col = mLimit - row
										tlSums(row, col) + brSums(row, col) - matrix(row, col)
									}
		) min
	}

	def
	bestSumsTL(matrix: Matrix, sums: Matrix, diag: Int, limit: Int): Unit = {
		if (diag == 0)
			sums(0)(0) = matrix(0, 0)
		else
		if (diag <= limit) {
			sums(diag)(0) = sums(diag - 1, 0) + matrix(diag)(0)
			for (row <- diag - 1 to 1 by -1) {
				val col = diag - row
				if (sums(row - 1, col) < sums(row, col - 1))
					sums(row)(col) = sums(row - 1, col) + matrix(row, col)
				else
					sums(row)(col) = sums(row, col - 1) + matrix(row, col)
			}
			sums(0)(diag) = sums(0, diag - 1) + matrix(0)(diag)
		}
		if (diag < limit)
			bestSumsTL(matrix, sums, diag + 1, limit)
	}

	def
	bestSumsBR(matrix: Matrix, sums: Matrix, diag: Int, limit: Int): Unit = {
		val mSize		= matrix.size
		val mLimit		= mSize - 1
		val brDiag		= mLimit * 2

		if (diag == brDiag)
			sums(mLimit)(mLimit) = matrix(mLimit, mLimit)
		else
		if (diag >= limit) {
			sums(mLimit)(mLimit - (brDiag - diag)) =  sums(mLimit, mLimit - (brDiag - diag) + 1) +
													matrix(mLimit, mLimit - (brDiag - diag))

			for (col <- mLimit - (brDiag - diag - 1) until mLimit) {
				val row = diag - col
				if (sums(row + 1, col) < sums(row, col + 1))
					sums(row)(col) = sums(row + 1, col) + matrix(row, col)
				else
					sums(row)(col) = sums(row, col + 1) + matrix(row, col)
			}

			sums(mLimit - (brDiag - diag))(mLimit) =  sums(mLimit - (brDiag - diag) + 1, mLimit) +
													matrix(mLimit - (brDiag - diag) ,    mLimit)
		}
		if (diag > limit)
			bestSumsBR(matrix, sums, diag - 1, limit)
	}


	def
	readMatrix(mtxFile: String): Matrix = {
		val mtxSource = Source.fromFile(mtxFile)
		val mtxBuffer = new ArrayBuffer[Array[Int]]

		mtxSource.getLines.foreach { line => mtxBuffer += line.split(",").map(_.toInt).toArray }
		mtxSource.close

		val m = mtxBuffer.toArray
		require(m.forall(_.length == m(0).length), "readMatrix: Matrix must have uniformly sized rows")
		Matrix(m)
	}

	def
	euler81(matrix: Matrix): Int =
		bestSum(matrix)

	def
	main(args: Array[String]): Unit =
		args foreach { mxFile =>
			val matrix = readMatrix(mxFile)
			val answer = euler81(matrix)
			printf("Euler81: \"%s\"; %d rows; %d columns => %d%n", mxFile, matrix.height, matrix.width, answer)
		}
}
