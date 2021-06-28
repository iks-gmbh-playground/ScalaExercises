package com.iks.codewars.caersarsCipher

import org.scalatest.flatspec.AnyFlatSpec
import FirstVariationSpec._
import com.iks.codewars.caesarsCipher.FirstVariation
import org.scalatest.Assertions.assertResult

class FirstVariationSpec extends AnyFlatSpec {
  it should "pass basic tests" in {
    val u1 = "I should have known that you would have a perfect answer for me!!!";
    val v1 = List("J vltasl rlhr ", "zdfog odxr ypw", " atasl rlhr p ", "gwkzzyq zntyhv", " lvz wp!!!");
    dotest1(u1, 1, v1)
    dotest2(v1, 1, u1)

    val u2 = "O CAPTAIN! my Captain! our fearful trip is done;";
    val v2 = List("T JIYDLUA!", " cp Vukpxg", "m! qxv lli", "apfx hgyg ","bm zlld;");
    dotest1(u2, 5, v2)
    dotest2(v2, 5, u2)
  }
}

object FirstVariationSpec {

  def dotest1(s: String, n: Int, exp: List[String]): Unit = {
    val actual: List[String] = FirstVariation.movingShift(s, n)
    assertResult(exp){actual}
  }
  def dotest2(v: List[String], n: Int, s: String): Unit = {
    val actual: String = FirstVariation.demovingShift(v, n)
    assertResult(s){actual}
  }
}