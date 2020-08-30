import org.scalatest._
import org.scalatest.Assertions._
import WeirdPrimeGenSpec._

class WeirdPrimeGenSpec extends FlatSpec {
  it should "pass Basic tests countOnes" in {
    testing1(1, 1);
    testing1(10, 8);
    testing1(100, 90);

  }
  it should "pass Basic tests maxPn" in {
    testing2(1, 5);
    testing2(5, 47);
    testing2(7, 101);

  }
  it should "pass Basic tests anOverAverage" in {
    testing3(5, 3);

  }
}

object WeirdPrimeGenSpec {
  def testing1(n: Long, expect: Long): Unit = {
    val actual = WeirdPrimeGen.countOnes(n)
    assertResult(expect){actual}
  }
  def testing2(n: Long, expect: Long): Unit = {
    val actual = WeirdPrimeGen.maxPn(n)
    assertResult(expect){actual}
  }
  def testing3(n: Long, expect: Int): Unit = {
    val actual = WeirdPrimeGen.anOverAverage(n)
    assertResult(expect){actual}
  }
}