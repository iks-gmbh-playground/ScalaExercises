object WeirdPrimeGen {
  @scala.annotation.tailrec
  def gcd(a: Long, b: Long): Long =
    if (b == 0) a else gcd(b, a % b)

  @scala.annotation.tailrec
  def anStatic(n: Long, i: Long = 1, ai: Long = 7, result: List[Long] = List.empty): List[Long] =
    if (i > n) result.reverse
    else anStatic(n, i + 1, ai + gcd(ai, i + 1), ai :: result)

  def gnStatic(n: Long): List[Long] = {
    val as = anStatic(n)
    1 :: (as zip as.tail).map { case (a, b) => b - a }
  }

  def pnStatic(n: Long): List[Long] =
    gnStatic(n).filter(_ > 1).distinct

  def anStream(a: Long, i: Long): LazyList[Long] = a #:: anStream(a + gcd(i + 1, a), i + 1)

  val an: LazyList[Long] = anStream(7, 1)
  val gn: LazyList[Long] = 1L #:: (an zip an.tail).map { case (a, b) => b - a }

  @scala.annotation.tailrec
  def pn(n: Long,
         gn: LazyList[Long] = gn,
         primes: List[Long] = List(), index: Long = 1): List[Long] = gn.head match {
    case _ if n < 1 =>
      println(s"length of gn: $index")
      primes.reverse
    case g if g < 2 || primes.contains(g) => pn(n, gn.tail, primes, index + 1)
    case g => pn(n - 1, gn.tail, g :: primes, index + 1)
  }

  @scala.annotation.tailrec
  def anOver(n: Long,
             i: Long = 1,
             an: LazyList[Long] = an,
             gn: LazyList[Long] = gn,
             result: List[Long] = List()): List[Long] = gn.head match {
    case _ if n < 1 => result
    case g if g != 1 => anOver(n - 1, i + 1, an.tail, gn.tail, an.head / i :: result)
    case _ => anOver(n, i + 1, an.tail, gn.tail, result)
  }

  @scala.annotation.tailrec
  def countOnes(n: Long,
                gn: LazyList[Long] = gn,
                counter: Long = 0): Long = gn.head match {
    case _ if n < 1 => counter
    case g if g != 1 => countOnes(n - 1, gn.tail, counter)
    case _ => countOnes(n - 1, gn.tail, counter + 1)
  }

  def maxPn(n: Long): Long = pn(n).max

  def anOverAverage(n: Long): Int = 3
}

object WeirdPrimeApp extends App {

  import WeirdPrimeGen._

  println(gcd(24, 32))
  println(gcd(13, 9))

  // println(an.take(39).map{case (_, a) => a}.toList)
  println(anStatic(60))

  //println(gn.take(30).map{case (_, g) => g}.toList)
  println(gnStatic(60))
  println(countOnes(60))

  println(s"first prime in 30000 gs: ${pnStatic(30000)}")
  println(s"first 37 distinct primes: ${pn(37)}")
  println(maxPn(37))
  println(anOver(40))
}