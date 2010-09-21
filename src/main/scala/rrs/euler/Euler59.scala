/*
	Euler59.scala

	Copyright © 2010 Randall R. Schulz. ALL RIGHTS RESERVED

	$Id$
*/

/*
	Each character on a computer is assigned a unique code and the preferred
	standard is ASCII (American Standard Code for Information Interchange). For
	example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

	A modern encryption method is to take a text file, convert the bytes to ASCII,
	then XOR each byte with a given value, taken from a secret key. The advantage
	with the XOR function is that using the same encryption key on the cipher text,
	restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

	For unbreakable encryption, the key is the same length as the plain text
	message, and the key is made up of random bytes. The user would keep the
	encrypted message and the encryption key in different locations, and without
	both "halves", it is impossible to decrypt the message.

	Unfortunately, this method is impractical for most users, so the modified
	method is to use a password as a key. If the password is shorter than the
	message, which is likely, the key is repeated cyclically throughout the
	message. The balance for this method is using a sufficiently long password
	key for security, but short enough to be memorable.

	Your task has been made easy, as the encryption key consists of three lower
	case characters. Using cipher1.txt <http://projecteuler.net/project/cipher1.txt>,
	a file containing the encrypted ASCII codes, and the knowledge that the plain
	text must contain common English words, decrypt the message and find the sum
	of the ASCII values in the original text.
*/

/*
	Output (wrapped):

	Euler22: "Euler59-cipher1.txt" => {'g'/103, 'o'/111, 'd'/100}; textSum=107359
	-==-
	(The Gospel of John, chapter 1) 1 In the beginning the Word already existed. He was with God,
	and he was God. 2 He was in the beginning with God. 3 He created everything there is. Nothing
	exists that he didn't make. 4 Life itself was in him, and this life gives light to everyone. 5
	The light shines through the darkness, and the darkness can never extinguish it. 6 God sent
	John the Baptist 7 to tell everyone about the light so that everyone might believe because
	of his testimony. 8 John himself was not the light; he was only a witness to the light. 9 The
	one who is the true light, who gives light to everyone, was going to come into the world. 10
	But although the world was made through him, the world didn't recognize him when he came. 11
	Even in his own land and among his own people, he was not accepted. 12 But to all who believed
	him and accepted him, he gave the right to become children of God. 13 They are reborn! This
	is not a physical birth resulting from human passion or plan, this rebirth comes from God.14
	So the Word became human and lived here on earth among us. He was full of unfailing love and
	faithfulness. And we have seen his glory, the glory of the only Son of the Father.
*/

package		rrs.euler


/**
	class Euler59
*/
object		Euler59
{
	import io.Source


	def
	readCypherText(cypherFile: String): Text = {
		val byteSplit = ","
		val cypherSource = Source.fromFile(cypherFile)
		val cypherContent = cypherSource.mkString.trim
		val cypherText = Array[Byte]((cypherContent split(byteSplit) map(_.toInt) map(_.toByte)): _*)
		cypherSource.close
		cypherText
	}

	type Text		= Array[Byte]
	type Pad		= Array[Byte]
	type PadSel		= (Int, Int, Int)

	val padBytes	= Array[Byte]('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
								  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
	val padSel0		= (0, 0, 0)
	val padSelZ		= (25, 25, 25)
	val padMax		= 25

	def
	pad(ps: PadSel): Pad =
		Array[Byte](padBytes(ps._1), padBytes(ps._2), padBytes(ps._3))

	def
	nextPadSel(ps: PadSel): PadSel =
		if (ps == padSelZ)
			padSel0
		else {
			var nps3 = ps._3 + 1
			var nps2 = ps._2
			var nps1 = ps._1

			if (nps3 > padMax) {
				nps3 = 0
				nps2 += 1
			}

			if (nps2 > padMax) {
				nps2 = 0
				nps1 += 1
			}

			(nps1, nps2, nps3)
		}

	def
	padSelStream(ps: PadSel): Stream[PadSel] =
		ps #:: (
			if (ps != padSelZ)
				padSelStream(nextPadSel(ps))
			else
				Stream.empty
		)

	def
	padSels: Stream[PadSel] =
		padSelStream(padSel0)


	def
	decrypt(text: Text, pad: Pad): Text = {
		val padSize = pad.length
		(text grouped(padSize) map(pg => (pg zip pad) map(cp => (cp._1 ^ cp._2).toByte))).toArray flatten
	}


	case
	class	TextInfo(cyper: Text, clear: Text, pad: Pad)
	{
		lazy val clearString	= asString

		val nChars				=  clear.length
		val nWords				= (clear count(b => b == ' ' || b == '\t' || b == '\n' || b == '\r')) + 1
		val nControl			=  clear count(b => b  < ' ' && b != '\t' && b != '\n' && b != '\r')

		def
		charsPerWord: Double =
			nChars.toDouble / nWords

		def
		nStopWords: Int =
			swRE.findAllIn(clearString).length

		def
		asString: String =
			new String(clear)
	}

	val maxCPW		= 10

	val stopWords	= List(	"a", "an", "the", "is", "of", "to"	)
	val swRE		= stopWords.mkString("\\b(", "|", ")\\b").r

	def
	winnow(cypherText: Text): TextInfo =
		(padSels
		 .map(ps => TextInfo(cypherText, decrypt(cypherText, pad(ps)), pad(ps)))
		 .filter(_.charsPerWord < maxCPW)
		 .max(Ordering.by((ti: TextInfo) => ti.nStopWords))
		)


	def
	euler59(cypherText: Text): TextInfo =
		winnow(cypherText)

	def
	showAnalysis(cypherText: Text): Unit = {
		printf("Cyphertext bytes: %d%n", cypherText.length)

		padSels foreach { ps =>
			val padN		= pad(ps)
			val clearText	= decrypt(cypherText, padN)
			val tInfo		= TextInfo(cypherText, clearText, padN)

			printf("ps=%s; nWords=%5d; wordSize=%8.3f; stop=%5d%n",
				   "{%2d, %2d, %2d}".format(ps._1, ps._2, ps._3),
				   tInfo.nWords, tInfo.charsPerWord, tInfo.nStopWords)
		}
	}

	def
	main(args: Array[String]): Unit =
		args foreach { arg =>
			val cypherBytes = readCypherText(arg)
			val answer = euler59(cypherBytes)
			val pad = answer.pad
			printf("Euler59: \"%s\" => %s; textSum=%d%n-==-%n%s%n%n", arg,
				   "{'%c'/%2d, '%c'/%2d, '%c'/%2d}".format(pad(0), pad(0), pad(1), pad(1), pad(2), pad(2)),
				   answer.clear map(_.toInt) sum,
				   answer.clearString)
		}
}
