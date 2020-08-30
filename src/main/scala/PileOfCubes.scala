object PileOfCubes {

  import math._

  def findNb(m: Long): Int =
    if (m < 0) -1
    else {
      // 1³ + 2³ + 3³ + ... n³ = (1 + 2 + 3 + ... n)²
      val n = sqrt(2 * sqrt(m) + 0.25) - 0.5
      if (n.toInt == n) n.toInt else -1
    }

  def findNbLoop(m: Long): Int = {
    @scala.annotation.tailrec
    def loop(n: Int, s: Long = 0): Int = {
      if (s == m) n
      else if (s > m) -1
      else loop(n + 1, s + pow(n+1, 3).toInt)
    }
    loop(0)
  }

}
