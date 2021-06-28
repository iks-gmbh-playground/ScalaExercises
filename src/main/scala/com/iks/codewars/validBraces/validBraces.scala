package com.iks.codewars.validBraces

import scala.annotation.tailrec

object validBraces {

  val closeBraces = ">)]}"
  val openClose = Map('{' -> '}', '[' -> ']', '(' -> ')', '<' -> '>')

  @tailrec
  def validBraces(str: String): Boolean =
    if (str.isEmpty) true
    else {
      val i = str.indexWhere(closeBraces.contains(_))
      if (i <= 0) false
      else if (openClose(str(i - 1)) != str(i)) false
      else validBraces(str.substring(0, i - 1) + str.substring(i + 1))
    }

}
