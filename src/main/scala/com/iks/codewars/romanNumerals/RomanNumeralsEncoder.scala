package com.iks.codewars.romanNumerals

import scala.annotation.tailrec

// Create a function taking a positive integer as its parameter and returning a string containing the Roman Numeral
// representation of that integer.
//
// Modern Roman numerals are written by expressing each digit separately starting with the left most digit and
// skipping any digit with a value of zero. In Roman numerals 1990 is rendered: 1000=M, 900=CM, 90=XC; resulting in MCMXC.
// 2008 is written as 2000=MM, 8=VIII; or MMVIII. 1666 uses each Roman symbol in descending order: MDCLXVI.
//
// Example:
//
// Roman.encode(1000) // should return "M"
// Help:
//
// Symbol    Value
// I          1
// V          5
// X          10
// L          50
// C          100
// D          500
// M          1,000
// Remember that there can't be more than 3 identical symbols in a row.
//
// More about roman numerals - http://en.wikipedia.org/wiki/Roman_numerals

object RomanNumeralsEncoder {

  val arabicToRoman = Map(1 -> "I", 5 -> "V", 10 -> "X", 50 -> "L", 100 -> "C", 500 -> "D", 1000 -> "M")

  @tailrec
  def encode(arabic: Int, one: Int = 1, result: String = ""): String = arabic match {
    case _ if arabic == 0 => result
    case _ if one == 1000 => arabicToRoman(1000) * arabic + result
    case _ =>
      val c = arabic % 10 match {
        case 0 => ""
        case i if i < 4 => arabicToRoman(one) * i
        case 4 => arabicToRoman(one) + arabicToRoman(one * 5)
        case 5 => arabicToRoman(one * 5)
        case i if i < 9 => arabicToRoman(one * 5) + arabicToRoman(one) * (i - 5)
        case 9 => arabicToRoman(one) + arabicToRoman(one * 10)
      }
      encode(arabic / 10, one * 10, c + result)
  }
}
