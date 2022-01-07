package com.iks.algorithms.gcd

import scala.annotation.tailrec
import scala.util.Random

object GCD extends App {

  def gcdNaive(a: Int, b: Int): Int =
    (a to 1 by -1).find(i => a % i == 0 && b % i == 0).getOrElse(1)

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else {
      val r = a % b
      gcd(b, r)
    }

  @tailrec
  def stressTest(maxN: Int, maxM: Int, f: (Int, Int) => Int, n: (Int, Int) => Int): Unit = {
    val a = Random.nextInt(maxN) + 1
    val b = Random.nextInt(maxM) + 1
    print(s"$a $b ")
    val expected = n(a, b)
    val actual = f(a, b)
    if (expected == actual) {
      println("OK")
      stressTest(maxN, maxM, f, n)
    } else println(s"wrong result: expected $expected but got $actual")
  }

  stressTest(50, 200, gcd, gcdNaive)

}
