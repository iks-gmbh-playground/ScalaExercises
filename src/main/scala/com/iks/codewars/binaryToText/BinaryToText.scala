package com.iks.codewars.binaryToText

import scala.annotation.tailrec

// Write a function that takes in a binary string and returns the equivalent decoded text (the text is ASCII encoded).
//
// Each 8 bits on the binary string represent 1 character on the ASCII table.
//
// The input string will always be a valid binary string.
//
// Characters can be in the range from "00000000" to "11111111" (inclusive)
//
// Note: In the case of an empty binary string your function should return an empty string.

object BinaryToText {

  @tailrec
  def binaryToString1(input: String, output: String = ""): String = input match {
    case "" => output
    case _ =>
      val char = input.take(8).foldLeft(0)((acc, c) => (acc << 1) + (if (c == '1') 1 else 0)).toChar
      binaryToString1(input.drop(8), output + char)
  }

  def binaryToString(input: String): String =
    input.grouped(8).map(_.foldLeft(0)((i, c) => (i << 1) + c.toString.toInt).toChar).mkString

  def binaryToStringBest(input: String): String =
    input.grouped(8).map(Integer.parseInt(_, 2).toChar).mkString

}
