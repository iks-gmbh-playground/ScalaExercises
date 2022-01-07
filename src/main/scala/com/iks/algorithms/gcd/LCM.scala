package com.iks.algorithms.gcd

import com.iks.algorithms.gcd.GCD.{gcd, stressTest}

object LCM extends App {
  // lcm(a, b) = (a * b) / gcd(a, b)

  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

  def lcmNaive(a: Int, b: Int): Int =
    (1 to a).find(_ * b % a == 0).map(_ * b).get

  stressTest(1000, 1000, lcm, lcmNaive)

}
