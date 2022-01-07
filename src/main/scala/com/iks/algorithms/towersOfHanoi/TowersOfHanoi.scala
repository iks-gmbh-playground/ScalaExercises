package com.iks.algorithms.towersOfHanoi

object TowersOfHanoi {
  var count = 0
  def move(n: Int, from: Int, to: Int): Unit = {
    if (n == 1) {
      println(s"move $n from peg $from to peg $to")
      count += 1
    }
    else {
      val unused = 6 - from - to
      move(n-1, from, unused)
      println(s"move $n from peg $from to peg $to")
      count += 1
      move(n-1, unused, to)
    }
  }
}
