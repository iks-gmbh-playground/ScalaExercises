import org.scalatest._
import org.scalatest.matchers.should._
import Meeting._

abstract class UnitSpec extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors

class MeetingTest extends UnitSpec {
  it should "convert a string of first names and last names" in {
    val s = "Hartwig:Toedter;Irene:Toedter;Willi:Heinrich"
    val expected = "(HEINRICH, WILLI)(TOEDTER, HARTWIG)(TOEDTER, IRENE)"
    val actual = meeting(s)
    assertResult(expected)(actual)
  }

  it should "result in an error if a is not equal b" in {
    val a = 2
    val b = 2
    assert(a == b)
  }
}
