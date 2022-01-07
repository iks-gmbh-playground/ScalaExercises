package com.iks.codewars.immortal

import org.scalatest._
import org.scalatest.matchers.should.Matchers

class ImmortalSpec extends FunSpec with Matchers with BeforeAndAfterAll {
  describe("Immortal.elderAge") {
    it("example") {
      assertResult(5) {
        Immortal.elderAge(8, 5, 1, 100)
      }
      assertResult(224) {
        Immortal.elderAge(8, 8, 0, 100007)
      }
      assertResult(11925) {
        Immortal.elderAge(25, 31, 0, 100007)
      }
      assertResult(4323) {
        Immortal.elderAge(5, 45, 3, 1000007)
      }
      // assertResult(1586){Immortal.elderAge(31, 39, 7, 2345)}
      // assertResult(808451){Immortal.elderAge(545, 435, 342, 1000007)}
      // You need to run this test very quickly before attempting the actual tests :)
      // assertResult(5456283){Immortal.elderAge(28827050410L, 35165045587L, 7109602, 13719506)}
    }
  }
}