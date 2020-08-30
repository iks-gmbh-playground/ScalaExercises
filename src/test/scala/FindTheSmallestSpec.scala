import org.scalatest.Assertions._
import org.scalatest.flatspec.AnyFlatSpec

class FindTheSmallestSpec extends AnyFlatSpec {

  import FindTheSmallestSpec._

  it should "pass basic tests" in {
    testing(261235, Array(126235, 2, 0))
    testing(256687587015L, Array(25668758715L, 9, 0))
    testing(935855753L, Array(358557539, 0, 8))
    testing(285365, Array(238565, 3, 1))
    testing(1234567, Array(1234567, 0, 0))
    testing(7654321, Array(1765432, 6, 0))
    testing(7238543, Array(2378543, 0, 2))
    testing(209917, Array(29917, 0, 1))
    testing(2312906993934201L, Array(231290699393421L, 14, 0))
    testing(199819884756L, Array(119989884756L, 4, 0))
    testing(123546, Array(123456, 3, 4))
    testing(6113411455659782L, Array(1134114556569782L, 0, 11))
    testing(6154771942778338L, Array(1546771942778338L, 0, 3))
  }
}

object FindTheSmallestSpec {
  private def testing(n: Long, exp: Array[Long]): Unit = {
    val act = FindTheSmallest.smallest(n)
    val actual: String = act.mkString(", ")
    val expect: String = exp.mkString(", ")
    println("Testing " + n)
    println("Actual --> " + actual)
    println("Expect --> " + expect)
    println("-")
    assertResult(expect) {
      actual
    }
  }
}

