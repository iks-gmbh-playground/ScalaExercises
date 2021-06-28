package com.iks.codewars.morsecode.decodeAdvanced

object MorseDecoder {
  val morseCodes = Map("...." -> "H",
    "." -> "E",
    ".---" -> "J",
    "..-" -> "U",
    "-.." -> "D",
    "-.--" -> "Y")

  def decodeBits(bits: String): String = {
    val trimmed = bits.reverse.dropWhile(_ == '0').
      reverse.dropWhile(_ == '0')
    val ones = trimmed.split("0+")
    val zeros = if (ones.size == 1) Array("") else trimmed.split("1+").tail :+ ""
    val unit = if (ones.size == 1) ones(0).length else (ones ++ zeros.init).map(_.length).min
    val DOT = "1" * unit
    val DASH = DOT * 3
    val GAP = "0" * unit
    val CHAR_GAP = GAP * 3
    val WORD_GAP = GAP * 7
    (ones.zip(zeros) map {
      case (DOT, GAP) => "."
      case (DOT, "") => "."
      case (DOT, CHAR_GAP) => ". "
      case (DOT, WORD_GAP) => ".   "
      case (DASH, GAP) => "-"
      case (DASH, "") => "-"
      case (DASH, CHAR_GAP) => "- "
      case (DASH, WORD_GAP) => "-   "
      case (d, g) => println(s"Error: '$d', '$g''")
    }).mkString
  }

  def decodeMorse(morseCode: String): String =
    morseCode.split("   ").map(_.split(" ").map(morseCodes).mkString).mkString(" ")
}