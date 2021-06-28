package com.iks.codewars.easyDiagonal

import org.scalatest.Assertions.assertResult
import org.scalatest.flatspec.AnyFlatSpec

class EasyDiagonalSpec extends AnyFlatSpec {
  import EasyDiagonalSpec._

  it should "pass basic tests" in {
    testing(20, 3, 5985)
    testing(20, 4,20349)
    testing(20, 5, 54264)
    testing(20, 15, 20349)
    testing(1291, 56, BigInt("15478983586799578981605681450735426444083026237083755223526535252775423299626083333014485638841019200"))

  }
}

object EasyDiagonalSpec {

  private def testing(n: Int, p: Int, expect: BigInt): Unit = {
    println("Testing: " + n + ", " + p)
    val actual: BigInt = EasyDiagonal.diagonal(n, p)
    println("Actual: " + actual)
    println("Expect: " + expect)
    println("*")
    assertResult(expect){actual}
  }
}

