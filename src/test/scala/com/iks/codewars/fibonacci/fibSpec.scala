package com.iks.codewars.fibonacci

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class fibSpec extends AnyFlatSpec with Matchers {
  "fib(0)" should "return 0" in {
    LargeFibonacciNumbers.fib(0) should be (0)
  }
  "fib(1)" should "return 1" in {
    LargeFibonacciNumbers.fib(1) should be (1)
  }
  "fib(-1)" should "return 1" in {
    LargeFibonacciNumbers.fib(-1) should be (1)
  }
  "fib(10)" should "return 55" in {
    LargeFibonacciNumbers.fib(10) should be (55)
  }
  "fib(-10)" should "return -55" in {
    LargeFibonacciNumbers.fib(-10) should be (-55)
  }
  "fib(5)" should "return 5" in {
    LargeFibonacciNumbers.fib(5) should be (5)
  }
  "fib(-5)" should "return 5" in {
    LargeFibonacciNumbers.fib(5) should be (5)
  }

}
