object FindX extends App {

  import math.{abs, pow, sqrt}

  def uLoop(x: Double, n: Long = 100000): Double =
    (1L to n).map(i => i * pow(x, i)).sum

  def u(x: Double, n: Long = 100000): Double =
    (x - n*pow(x, n+1) + n*pow(x, n+2))/pow(1-x, 2)

  def estimates(x: Double, s0: Double, i: Long = 2): LazyList[(Double, Double)] = {
    val s1 = u(x, i)
    (s0, s1) #:: estimates(x, s1, i+1)
  }

  @scala.annotation.tailrec
  def solveHTO(m: Double, lower: Double = 0.0, upper: Double = 1.0, epsilon: Double = 1e-12): Double = {
    (lower + upper) / 2.0 match {
      case guess if abs(upper - guess) < epsilon => guess
      case guess if u(guess) < m => solveHTO(m, guess, upper)
      case guess => solveHTO(m, lower, guess)
    }
  }

  def solve(m: Double): Double =
    1 + (1 - sqrt(4*m + 1)) / (2 * m)

  def estimatesWithIndex(x: Double, s0: Double, i: Long = 2): LazyList[(Long, Double, Double)] = {
    val s1 = u(x, i)
    (i, s0, s1) #:: estimatesWithIndex(x, s1, i+1)
  }

//  println(s"${u(10, 0.03125)} - ${uFormula(10, 0.03125)}")
//  println(s"${u(10, 0.0625)} - ${uFormula(10, 0.0625)}")
//  println(s"${u(10, 0.125)} - ${uFormula(10, 0.125)}")
//  println(s"${u(10, 0.25)} - ${uFormula(10, 0.25)}")
//  println(s"${u(10, 0.5)} - ${uFormula(10, 0.5)}")

//  val (i, e0, e1) = (estimatesWithIndex(0.9175577529658328, 0.9175577529658328) takeWhile { case (_, e0, e1) => math.abs(e0 - e1) > 5e-13 }).toList.last
//  println(s"after $i iterations the necessary precision was reached: $e1 (${math.abs(e1 - 135.0)})")

  println(s"let m be 0.50 than x is ${solve(0.03125)}")
  println(s"let m be 0.50 than x is ${solve(0.0625)}")
  println(s"let m be 0.50 than x is ${solve(0.125)}")
  println(s"let m be 0.50 than x is ${solve(0.25)}")
  println(s"let m be 0.50 than x is ${solve(0.50)}")
  println(s"let m be 1.00 than x is ${solve(1.0)}")
  println(s"let m be 2.00 than x is ${solve(2.0)}")
  println(s"let m be 4.00 than x is ${solve(4.0)}")
  println(s"let m be 5.00 than x is ${solve(5.0)}")
  println(s"let m be 8.00 than x is ${solve(8.0)}")
  println(s"let m be 9.00 than x is ${solve(9.0)}")
  println(s"let m be 13.00 than x is ${solve(13.0)}")
  println(s"let m be 135.00 than x is ${solve(135.0)}")
  println(s"let m be 335.00 than x is ${solve(335.0)}")
  println(s"let m be 435.00 than x is ${solve(435.0)}")
  println(s"let m be 535.00 than x is ${solve(535.0)}")
  println(s"let m be 635.00 than x is ${solve(635.0)}")
  println(s"let m be 735.00 than x is ${solve(735.0)}")
  println(s"let m be 835.00 than x is ${solve(835.0)}")
  println(s"let m be 935.00 than x is ${solve(935.0)}")
  println(s"let m be 1531.00 than x is ${solve(1531.0)}")
  println(s"let m be 10000.00 than x is ${solve(10000.0)}")
  println(s"let m be 100000.00 than x is ${solve(100000.0)}")
  println(s"let m be 500000.00 than x is ${solve(500000.0)}")
}
