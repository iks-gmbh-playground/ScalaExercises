package com.iks.codewars.howMuch

import scala.::

object Carboat {
  def range(min: Int, max: Int): Range =
    if (min < 37) Range.inclusive(37, max, 63)
    else Range.inclusive(((min-37)/63.toDouble).ceil.toInt * 63 + 37, max, 63)

  def howmuch(m: Int, n: Int): String =
    range(m.min(n), n.max(m)).
      map(i => s"""["M: $i", "B: ${(i - 2) / 7}", "C: ${(i - 1) / 9}"]""").
      mkString("[", ", ", "]")
}
