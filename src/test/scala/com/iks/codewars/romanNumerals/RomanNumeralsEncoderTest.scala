package com.iks.codewars.romanNumerals

import org.scalatest.funsuite.AnyFunSuite

class RomanNumeralsEncoderTest extends AnyFunSuite {
  test("Samples") {
    assert(RomanNumeralsEncoder.encode(1) === "I")
    assert(RomanNumeralsEncoder.encode(3) === "III")
    assert(RomanNumeralsEncoder.encode(4) === "IV")
    assert(RomanNumeralsEncoder.encode(6) === "VI")
    assert(RomanNumeralsEncoder.encode(14) === "XIV")
    assert(RomanNumeralsEncoder.encode(21) === "XXI")
    assert(RomanNumeralsEncoder.encode(89) === "LXXXIX")
    assert(RomanNumeralsEncoder.encode(91) === "XCI")
    assert(RomanNumeralsEncoder.encode(984) === "CMLXXXIV")
    assert(RomanNumeralsEncoder.encode(1000) === "M")
    assert(RomanNumeralsEncoder.encode(1666) === "MDCLXVI")
    assert(RomanNumeralsEncoder.encode(1889) === "MDCCCLXXXIX")
    assert(RomanNumeralsEncoder.encode(1989) === "MCMLXXXIX")
    assert(RomanNumeralsEncoder.encode(2008) === "MMVIII")
    assert(RomanNumeralsEncoder.encode(3008) === "MMMVIII")
    assert(RomanNumeralsEncoder.encode(5008) === "MMMMMVIII")
  }
}
