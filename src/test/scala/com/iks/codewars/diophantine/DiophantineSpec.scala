package com.iks.codewars.diophantine

import org.scalatest.flatspec.AnyFlatSpec
import DiophantineSpec._
import org.scalatest.Assertions.assertResult

class DiophantineSpec extends AnyFlatSpec {
  it should "pass basic tests" in {
    testing(5, "[(3, 1)]")
    testing(9005, "[(4503, 2251), (903, 449)]")
    testing(9008, "[(1128, 562)]")
  }
}

object DiophantineSpec {

  private def testing(n: Long, expect: String): Unit = {
    println("Testing:  " + n)
    val actual: String = Diophantine.solEquaStr(n)
    println("Actual: " + actual)
    println("Expect: " + expect)
    println("-")
    assertResult(expect) {
      actual
    }
  }
}
