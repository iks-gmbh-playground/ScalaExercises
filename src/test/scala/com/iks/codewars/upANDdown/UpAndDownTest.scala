package com.iks.codewars.upANDdown

import com.iks.codewars.upANDdown.UpAndDownTest.testing
import org.scalatest._
import org.scalatest.Assertions._


class UpAndDownTest extends FlatSpec {
  it should "pass basic tests" in {
    testing("", "") // 0
    testing("who hit retaining The That a we taken", "who RETAINING hit THAT a THE we TAKEN") // 3
    testing("on I came up were so grandmothers", "i CAME on WERE up GRANDMOTHERS so") // 4
    testing("way the my wall them him", "way THE my WALL him THEM") // 1
    testing("turn know great-aunts aunt look A to back", "turn GREAT-AUNTS know AUNT a LOOK to BACK") // 2

  }
}

object UpAndDownTest {
  private def testing(s: String, expect: String): Unit = {
    val actual = UpAndDown.arrange(s)
    println("Testing: " + s)
    println("Actual --> " + actual)
    println("Expect --> " + expect)
    println("-")
    assertResult(expect){actual}
  }
}

