package com.iks.codewars.trailingZeros

import scala.annotation.tailrec

// Write a function zeros that returns the number of trailing zeros in a factorial. Examples:
//
// factorial(7) == 5040 => zeros(7) == 0
// factorial(12) == 479001600 => zeros(12) == 2
// factorial(15) == 1307674368000 => zeros(15) == 3
// factorial(250) == (Number of 493 digits) => zeros(250) == 62

object TrailingZeros {
  @tailrec
  def factorial(n: Long, result: BigInt = 1): BigInt = n match {
    case i if i < 1 => result
    case i => factorial(n - 1, result * i)
  }

  @tailrec
  def zeros(n: Long, result: Long = 0): Long = n / 5 match {
    case 0 => result
    case x => zeros(x, result + x)
  }

  def myZeros(n: Long): Long = {
    val log5n = (math.log(n) / math.log(5)).round
    (1L to log5n).map(n / math.pow(5, _).toLong).sum
  }
}
