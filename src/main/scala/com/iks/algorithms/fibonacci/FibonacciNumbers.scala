package com.iks.algorithms.fibonacci

import scala.annotation.tailrec

object FibonacciNumbers {

  def fibonacciNaive(n: Int): Long = n match {
    case 0 | 1 => n
    case i if i > 1 => fibonacciNaive(n - 1) + fibonacciNaive(n - 2)
    case _ => throw new IllegalArgumentException
  }

  def fibonacci(n: Int): BigInt = n match {
    case _ if n < 0 => throw new IllegalArgumentException
    case 0 | 1 => n
    case _ => (2 to n).foldLeft((BigInt(0), BigInt(1))) { case ((nMinusTwo, nMinus0ne), _) => (nMinus0ne, nMinus0ne + nMinusTwo) }._2
  }

  def fibonacciLastDigit(n: Int): Int = n match {
    case _ if n < 0 => throw new IllegalArgumentException
    case 0 | 1 => n
    case _ => (2 to n).foldLeft((0, 1)) { case ((nMinusTwo, nMinus0ne), _) => (nMinus0ne, (nMinus0ne + nMinusTwo) % 10) }._2
  }

  def pisanoPeriod(x: Int): IndexedSeq[Int] = {
    @tailrec
    def loop(ps: IndexedSeq[Int], n: Int): IndexedSeq[Int] =
      if (2 * n > ps.length) IndexedSeq.empty
      else if (ps.take(n) == ps.slice(n, n + n)) ps.take(n)
      else loop(ps, n + 1)

    if (x == 1) Vector(0, 1)
    else loop(pisanoSequence(x, 2 * x * x), x)
  }

  def pisanoSequence(m: Int, n: Int): IndexedSeq[Int] = {
    if (n == 1) Vector(0, 1)
    else (2 to n).foldLeft(Vector(0, 1))((acc, i) => acc :+ (acc(i - 2) + acc(i - 1)) % m)
  }

  def fibonacciModM(n: Long, m: Int = 10): Int = {
    val pp = pisanoPeriod(math.min(m, n).toInt)
    pp((n % pp.length).toInt) % m
  }

  def fibonacciModMNaive(n: Long, m: Int = 10): Int = {
    assert(n >= 0)
    assert(2 <= m && m <= 1e5)

    @tailrec
    def loop(i: Long, nMinusTwo: Int = 0, nMinusOne: Int = 1): Int =
      if (i > n) nMinusOne
      else loop(i + 1, nMinusOne, (nMinusOne + nMinusTwo) % m)

    n match {
      case 0 => 0
      case 1 => 1
      case _ => loop(2)
    }
  }

  def run[A](f: => A): Long = {
    val start = System.nanoTime()
    println(f)
    val stop = System.nanoTime()
    stop - start
  }
}
