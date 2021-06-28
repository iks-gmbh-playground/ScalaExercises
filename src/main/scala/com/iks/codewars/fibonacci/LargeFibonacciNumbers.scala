package com.iks.codewars.fibonacci

import scala.annotation.tailrec

// In this kata you will have to calculate fib(n) where:
//
// fib(0) := 0
// fib(1) := 1
// fin(n + 2) := fib(n + 1) + fib(n)
//
// Write an algorithm that can handle n up to 2000000.
//
// Your algorithm must output the exact integer answer, to full precision.
// Also, it must correctly handle negative numbers as input.
//
// HINT I: Can you rearrange the equation fib(n + 2) = fib(n + 1) + fib(n) to find fib(n)
// if you already know fib(n + 1) and fib(n + 2)?
// Use this to reason what value fib has to have for negative values.
//
// HINT II: See https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.4

object LargeFibonacciNumbers {
  @tailrec
  def fib2(n: Int,
           b: BigInt = 0, a: BigInt = 1,
           p: BigInt = 0, q: BigInt = 1): BigInt = n match {
    case 0 => b
    case _ if n % 2 == 0 => fib2(n / 2, b, a, p*p + q*q, q*q + 2*p*q)
    case _ => fib2(n - 1, b*p + a*q , b*q + a*q + a*p, p, q)
  }

  def fib(n: Int): BigInt =
    if (n < 0 && n % 2 == 0) -fib2(-n)
    else fib2(math.abs(n))
}
