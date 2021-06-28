package com.iks.codewars.integerRanges

import com.iks.codewars.integerRanges.RangeExtraction.extractRanges
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RangeExtractionSpec extends AnyFlatSpec with Matchers{

  "-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20" should "return -6,-3-1,3-5,7-11,14,15,17-20" in {
    val list = List(-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20)
    val expected = "-6,-3-1,3-5,7-11,14,15,17-20"

    extractRanges(list) shouldBe expected
  }
}
