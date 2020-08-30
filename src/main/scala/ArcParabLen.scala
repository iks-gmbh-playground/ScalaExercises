object ArcParabLen extends App {

  import math._

  def pythagoras(i: Long, h: Double): Double =
    sqrt(pow(i * h - (i + 1) * h, 2) + pow(i * i * h * h - pow((i + 1) * h, 2), 2))

  def lenCurve(n: Int): Double =
    (0 until n).foldLeft(0.0)((z, i) => z + pythagoras(i, 1.0 / n))

  println(lenCurve(10000))
  println(lenCurve(100000))
}