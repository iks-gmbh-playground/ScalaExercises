package com.iks.codewars.binaryToText

import org.scalatest.flatspec.AnyFlatSpec

class BinaryToTextSpec extends AnyFlatSpec{
  it should "work with example tests" in {
    assert(BinaryToText.binaryToString("0100100001100101011011000110110001101111") == "Hello")
    assert(BinaryToText.binaryToString("00110001001100000011000100110001") == "1011")
  }

}
