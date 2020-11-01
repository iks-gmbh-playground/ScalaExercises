import org.scalatest._
import org.scalatest.Assertions._

import TankTruckTest._

class TankTruckTest extends FlatSpec {
  it should "pass basic tests" in {
    doTest(5, 7, 3848, 2940)
    doTest(2, 7, 3848, 907)
    doTest(3, 6, 3500, 1750)

  }
}

object TankTruckTest {

  private def doTest(hg: Int, d: Int, vt: Int, expect: Int): Unit = {
    println("Testing:\n" + hg + ", " + d + ", " + vt)
    val actual: Int = TankTruck.tankVol(hg, d, vt)
    println("Actual: " + actual)
    println("Expect: " + expect)
    println("*")
    assertResult(expect){actual}
  }
}