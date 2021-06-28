package com.iks.codewars.diophantine

// In mathematics, a Diophantine equation is a polynomial equation, usually with two or more unknowns,
// such that only the integer solutions are sought or studied.
//
// In this kata we want to find all integers x, y (x >= 0, y >= 0) solutions of a diophantine equation of the form:
//
// x2 - 4 * y2 = n
// (where the unknowns are x and y, and n is a given positive number) in decreasing order of the positive xi.
//
// If there is no solution return [] or "[]" or "". (See "RUN SAMPLE TESTS" for examples of returns).
//
// Examples:
// solEquaStr(90005) --> "[[45003, 22501], [9003, 4499], [981, 467], [309, 37]]"
// solEquaStr(90002) --> "[]"
//
// Hint:
// x2 - 4 * y2 = (x - 2*y) * (x + 2*y)

object Diophantine {

  def solEquaStr(n: Long): String = {
    val decide = new PartialFunction[Long, (Long, Long)] {
      override def isDefinedAt(b: Long): Boolean = {
        val a = n / b
        n % b == 0 && (a - b) % 4 == 0 && (a + b) % 2 == 0
      }

      override def apply(b: Long): (Long, Long) = {
        val a = n / b
        ((a + b) / 2, (a - b) / 4)
      }
    }

    (1L to math.sqrt(n).toLong) collect (decide) map { case (a, b) => s"($a, $b)" } mkString("[", ", ", "]")
  }

  def solEquaStrBetter(n: Long): String =
    (for {
      b <- 1L to math.sqrt(n).toLong
      if n % b == 0
      a = n / b
      if (a + b) % 2 == 0
      if (a - b) % 4 == 0
    } yield (a, b)) map { case (a, b) => s"($a, $b)"} mkString("[", ", ", "]")
}


