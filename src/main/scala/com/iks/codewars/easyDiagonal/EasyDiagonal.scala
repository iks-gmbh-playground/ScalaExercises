package com.iks.codewars.easyDiagonal

import scala.annotation.tailrec

// In the drawing below we have a part of the Pascal's triangle, lines are numbered from zero (top).
// The left diagonal in pale blue with only numbers equal to 1 is diagonal zero,
// then in dark green (1, 2, 3, 4, 5, 6, 7) is diagonal 1,
// then in pale green (1, 3, 6, 10, 15, 21) is diagonal 2 and so on.
//
// We want to calculate the sum of the binomial coefficients on a given diagonal.
// The sum on diagonal 0 is 8 (we'll write it S(7, 0), 7 is the number of the line where we start,
// 0 is the number of the diagonal). In the same way S(7, 1) is 28, S(7, 2) is 56.
//
// Can you write a program which calculate S(n, p) where n is the line where we start and p is
// the number of the diagonal?
//
// The function will take n and p (with: n >= p >= 0) as parameters and will return the sum.
//
// Examples:
// diagonal(20, 3) => 5985
// diagonal(20, 4) => 20349
//
// Hint:
// When following a diagonal from top to bottom have a look at the numbers on the diagonal at its right.
//
// Ref:
// http://mathworld.wolfram.com/BinomialCoefficient.html

object EasyDiagonal {

  def run[A](f: => A): A = {
    val t0 = System.currentTimeMillis
    val result = f
    println(s"duration: ${(System.currentTimeMillis - t0)/1000d} seconds.")
    result
  }

  @tailrec
  def product(from: Int, to: Int, result: BigInt = BigInt(1)): BigInt = (from, to) match {
    case (f, t) if f > t => result
    case (f, t) if f <= 0 => BigInt(0)
    case (f, t) => product(f + 1, t, result * f)
  }

  def myBinomial(n: Int, k: Int): BigInt =
    product(k + 1, n) / product(1, n - k)

  @tailrec
  def binomial(n: Int, k: Int, f: BigInt = 1, d: BigInt = 1): BigInt = k match {
    case _ if k > n => BigInt(0)
    case _ if k == 0 => f * BigInt(1) / d
    case _ if k > n/2 => binomial(n, n - k, f, d)
    case _ => binomial(n - 1, k - 1, f * n, d * k)
  }

  def diagonalSlow(n: Int, p: Int): BigInt =
    (p to n).map(i => binomial(i - 1, p) + binomial(i - 1, p - 1)).sum - 1

  def diagonalFaster(n: Int, p: Int): BigInt =
    (p + 1 to n).foldLeft((BigInt(1), BigInt(1)))((acc, i) => {
      val next = acc._2 + binomial(i-1, p - 1)
      (acc._1 + next, next)
    })._1

  // best and fastest solution

  def choose(n: Int, k: Int): BigInt =
    (1 to k).foldLeft(BigInt(1))((acc, i) => acc * (n - i + 1) / i)

  def diagonal(n: Int, p: Int): BigInt =
    choose(n + 1, p + 1)
}
