package com.iks.codewars.binomials

// The purpose of this kata is to write a program that can do some algebra.
// Write a function expand that takes in an expression with a single, one character variable, and expands it.
// The expression is in the form (ax+b)^n where a and b are integers which may be positive or negative,
// x is any single character variable, and n is a natural number.
// If a = 1, no coefficient will be placed in front of the variable.
// If a = -1, a "-" will be placed in front of the variable.
//
// The expanded form should be returned as a string in the form ax^b+cx^d+ex^f... where a, c, and e
// are the coefficients of the term, x is the original one character variable that was passed in the original
// expression and b, d, and f, are the powers that x is being raised to in each term and are in decreasing order.
// If the coefficient of a term is zero, the term should not be included.
// If the coefficient of a term is one, the coefficient should not be included.
// If the coefficient of a term is -1, only the "-" should be included.
// If the power of the term is 0, only the coefficient should be included.
// If the power of the term is 1, the caret and power should be excluded.
//
// Examples:
// BinomialExpansion.expand("(x+1)^2")      // returns "x^2+2x+1"
// BinomialExpansion.expand("(p-1)^3")      // returns "p^3-3p^2+3p-1"
// BinomialExpansion.expand("(2f+4)^6")     // returns "64f^6+768f^5+3840f^4+10240f^3+15360f^2+12288f+4096"
// BinomialExpansion.expand("(-2a-4)^0")    // returns "1"
// BinomialExpansion.expand("(-12t+43)^2")  // returns "144t^2-1032t+1849"
// BinomialExpansion.expand("(r+0)^203")    // returns "r^203"
// BinomialExpansion.expand("(-x-1)^2")     // returns "x^2+2x+1"
//
// Note for Scala Users:
// For the generalized equation (ax+b)^n,
//
// a ranges from -100 to 100
// b ranges from -100 to 100
// n ranges from 0 to 10

object BinomialExpansion {

  import scala.annotation.tailrec

  def parse(expr: String): (Int, String, Int, Int) = {
    val binomialRegEx = "\\((-?\\d*)([A-Za-z])([+-]\\d*)\\)\\^(\\d*)".r
    val binomialRegEx(a, x, b, n) = expr
    a match {
      case "" => (1, x, b.toInt, n.toInt)
      case "-" => (-1, x, b.toInt, n.toInt)
      case _ => (a.toInt, x, b.toInt, n.toInt)
    }
  }

  @tailrec
  def pascalLine(n: Int, i: Int = 0, result: Vector[Long] = Vector(1L)): Vector[Long] =
    if (i >= n) result
    else pascalLine(n, i + 1, (0L +: result :+ 0L sliding 2 map (_.sum)).toVector)

  def expand(expr: String): String = {
    val (a, variable, b, exponent) = parse(expr)
    if (exponent == 0) "1"
    else {
      val terms = pascalLine(exponent).zipWithIndex map {
        case (c, i) => (BigInt(a).pow(exponent - i) * BigInt(b).pow(i) * c, exponent - i)
      } map { case (coefficient, exp) =>
        val sign = if (coefficient > 0 && exp != exponent) "+" else ""
        (coefficient.toString, exp) match {
          case ("0", _) => ""
          case (c, 0) => s"$sign$c"
          case ("1", 1) => s"$sign$variable"
          case ("-1", 1) => s"-$variable"
          case ("1", i) => s"$sign$variable^$i"
          case ("-1", i) => s"-$variable^$i"
          case (c, 1) => s"$sign$c$variable"
          case (c, i) => s"$sign$c$variable^$i"
        }
      }
      terms.mkString
    }
  }
}
