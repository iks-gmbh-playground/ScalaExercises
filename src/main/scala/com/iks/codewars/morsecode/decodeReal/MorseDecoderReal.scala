package com.iks.codewars.morsecode.decodeReal

object MorseCodes {
  val MORSE_CODE: Map[String, Char] = Map(
    ".-" -> 'A',
    "_..." -> 'B',
    "_._." -> 'C',
    "-.." -> 'D',
    "." -> 'E',
    ".._." -> 'F',
    "__." -> 'G',
    "...." -> 'H',
    ".." -> 'I',
    ".---" -> 'J',
    "_._" -> 'K',
    "__" -> 'M',
    "..-" -> 'U',
    "-.--" -> 'Y',
    "__.." -> 'Z')
}

object MorseDecoderReal {

  import MorseCodes.MORSE_CODE
  import scala.annotation.tailrec

  @tailrec
  def countSequences(bits: String, result: Seq[(Char, Int)] = Seq.empty): Seq[(Char, Int)] =
    bits match {
      case "" => result
      case s"1$_" => countSequences(bits.dropWhile(_ == '1'), result :+ ('1', bits.takeWhile(_ == '1').length))
      case s"0$_" => countSequences(bits.dropWhile(_ == '0'), result :+ ('0', bits.takeWhile(_ == '0').length))
      case _ => throw new IllegalArgumentException(bits)
    }

  def selectShortSequences(symbols: Seq[Int]): Seq[Int] =
    if (symbols.size > 1) {
      val sortedValues = symbols.sorted
      val diffs = sortedValues.zip(sortedValues.tail).map { case (a, b) => b - a }
      val index = diffs.indexWhere(_ > 1)
      sortedValues.splitAt(index + 1)._1
    } else symbols

  def estimateRate(countedSequences: Seq[(Char, Int)]): Int = {
    val frequencies = countedSequences.sortBy(_._2).map(_._2).distinct
    frequencies.size
  }

  def nextTo137(c: Char, r: Int, rate: Double): Int = {
    val diffs = if (c == '0')
      List(math.abs(r - rate), math.abs(r - 3 * rate), math.abs(r - 7 * rate))
    else
      List(math.abs(r - rate), math.abs(r - 3 * rate))
    diffs.indexOf(diffs.min) match {
      case 0 => 1
      case 1 => 3
      case 2 => 7
    }
  }

  def trimNulls(bits: String): String =
    bits.dropWhile(_ == '0').reverse.dropWhile(_ == '0').reverse

  def decodeBitsAdvanced(bits: String): String = {
    println(bits)
    val counted = countSequences(trimNulls(bits))
    val onesCounted = counted.filter(_._1 == '1').map(_._2)
    val nullsCounted = counted.filter(_._1 == '0').map(_._2)
    val shortOnes = selectShortSequences(onesCounted)
    val shortNulls = selectShortSequences(nullsCounted)
    val guessedRate = math.min(shortOnes.sum / shortOnes.size.toDouble, shortNulls.sum / shortNulls.size.toDouble)
    (counted map { case (c, r) => (c, nextTo137(c, r, guessedRate)) match {
      case ('1', 1) => "."
      case ('1', 3) => "-"
      case ('0', 1) => ""
      case ('0', 3) => " "
      case ('0', 7) => "   "
    }
    }).mkString
  }

  def decodeMorse(morseCode: String): String = {
    if (morseCode.isEmpty) ""
    else
      morseCode.
        split("  +").
        map(_.split(" ").
          map(MORSE_CODE).
          mkString).
        mkString(" ")
  }
}
