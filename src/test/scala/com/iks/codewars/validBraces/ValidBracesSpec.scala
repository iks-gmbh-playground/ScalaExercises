package com.iks.codewars.validBraces

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValidBracesSpec extends AnyFlatSpec with Matchers {
  import validBraces._

  "()" should "be a valid expression" in {
    validBraces("()") shouldBe true
  }

  "[(])" should "be an invalid expression" in {
    validBraces("[(])") shouldBe false
  }

  ")]}" should "be an invalid expression" in {
    validBraces(")]}") shouldBe false
  }

  "([{" should "be an invalid expression" in {
    validBraces("([{") shouldBe false
  }

  "[({})](]" should "be an invalid expression" in {
    validBraces("[({})](]") shouldBe false
  }

  "[({})]()" should "be a valid expression" in {
    validBraces("[({})]()") shouldBe true
  }


}
