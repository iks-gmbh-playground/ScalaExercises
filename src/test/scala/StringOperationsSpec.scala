import org.scalatest._
import org.scalatest.Assertions._

import StringOperationsSpec._

class StringOperationsSpec extends FlatSpec{
  import StringOperations._
  it should "pass basic tests operArray rot90Clock" in {
    testing(oper(rot90Clock, "rgavce\nvGcEKl\ndChZVW\nxNWgXR\niJBYDO\nSdmEKb"),
      "Sixdvr\ndJNCGg\nmBWhca\nEYgZEv\nKDXVKc\nbORWle")
  }

  it should "pass basic tests operArray diag1Sym" in {
    testing(oper(diag1Sym, "wuUyPC\neNHWxw\nehifmi\ntBTlFI\nvWNpdv\nIFkGjZ"),
      "weetvI\nuNhBWF\nUHiTNk\nyWflpG\nPxmFdj\nCwiIvZ")
  }

  it should "pass basic tests operArray selfieAndDiag1" in {
    testing(oper(selfieAndDiag1, "NJVGhr\nMObsvw\ntPhCtl\nsoEnhi\nrtQRLK\nzjliWg"),
      "NJVGhr|NMtsrz\nMObsvw|JOPotj\ntPhCtl|VbhEQl\nsoEnhi|GsCnRi\nrtQRLK|hvthLW\nzjliWg|rwliKg");
  }
}

object StringOperationsSpec {
  private def testing(actual: String, expect: String): Unit = {
    println("Actual: " + actual)
    println("Expect: " + expect)
    println("-")
    assertResult(expect){actual}
  }
}
